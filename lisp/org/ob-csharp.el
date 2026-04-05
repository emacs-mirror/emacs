;;; ob-csharp.el --- org-babel functions for csharp evaluation -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author:     Maximilian Kueffner
;; Maintainer: Maximilian Kueffner <poverobuosodonati@gmail.com>
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

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

;;; Requirements:

;; Some .NET runtime environment should be installed.
;; The `dotnet' command should be available to the system's environment
;; (PATH discoverable for example).

;;; Code:
(require 'ob)

;; file extension for C#
(add-to-list 'org-babel-tangle-lang-exts '("csharp" . "cs"))

;; default header arguments for C#
(defvar org-babel-default-header-args:csharp
  '((main . ((no)))
    (nugetconfig . :any)
    (framework . :any)
    (class . ((no nil :any)))
    (references . :any)
    (usings . :any)
    (cmdline . :any))
  "Csharp specific header arguments.")

(defcustom org-babel-csharp-compiler "dotnet"
  "The program to call for compiling a csharp project."
  :group 'org-babel
  :package-version '(Org. "9.8")
  :type 'string
  :risky t)

(defun org-babel-csharp--default-compile-command (dir-proj-sln bin-dir)
  "Construct the default compilation command for C#.

DIR-PROJ-SLN is either a directory containing a \".csproj\" or \".sln\" file
or a full path to either of these.
BIN-DIR is the directory for the compiled output."
  (format "%s build --output %S %S"
          org-babel-csharp-compiler bin-dir dir-proj-sln))

(defun org-babel-csharp--default-restore-command (project-file)
  "Construct the default restore command for C# projects.

PROJECT-FILE is a path to a \".csproj\" file on which the restore command
takes effect."
  (format "%s restore %S" org-babel-csharp-compiler project-file))

(defun org-babel-csharp--find-dotnet-version ()
  "Get a list of dotnet major versions from a list of dotnet sdks."
  (cl-delete-if #'(lambda (v) (= 0 v))
             (delete-dups
              (mapcar #'(lambda (n)
                          (let ((fr (string-match "^[0-9.]+\\." n))
                                (to (string-match "\\." n)))
                            (string-to-number (substring n fr to))))
                      (split-string
                       (shell-command-to-string
                        (format "%s --list-sdks" org-babel-csharp-compiler))
                       "\n")))))

(defcustom org-babel-csharp-default-target-framework
  (format "net%s.0"
          (let ((net-sdks (org-babel-csharp--find-dotnet-version)))
            (when net-sdks
              (apply #'max net-sdks))))
  "The desired target framework to use."
  :group 'org-babel
  :package-version '(Org. "9.8")
  :type 'string
  :safe #'stringp)

(defcustom org-babel-csharp-generate-compile-command
  #'org-babel-csharp--default-compile-command
  "A function creating the compile command.

It must take two parameters intended for the target binary directory and
a .sln file, .csproj file, or a base directory where either can be found."
  :group 'org-babel
  :package-version '(Org. "9.8")
  :type 'function
  :risky t)

(defcustom org-babel-csharp-generate-restore-command
  #'org-babel-csharp--default-restore-command
  "A function creating a project restore command.

It must take one parameter defining the project to perform a restore on."
  :group 'org-babel
  :package-version '(Org. "9.8")
  :type 'function
  :risky t)

(defcustom org-babel-csharp-additional-project-flags nil
  "Will be passed in the \"PropertyGroup\" defining the project.

This is taken as-is. It should be a string in XML-format."
  :group 'org-babel
  :package-version '(Org. "9.8")
  :type '(choice string (const nil))
  :safe (lambda (x) (or (eq x nil) (stringp x))))

(defun org-babel-csharp--generate-project-file (refs framework)
  "Generate the file content to be used in a csproj-file.

REFS is a list of references. Check `org-babel-csharp--format-refs' for
the allowed semantics.
FRAMEWORK is the target framework."
  (unless framework
    (error "framework cannot be nil"))
  (concat "<Project Sdk=\"Microsoft.NET.Sdk\">\n\n  "
          (when refs
            (org-babel-csharp--format-refs refs))
          "\n\n  <PropertyGroup>"
          "\n    <OutputType>Exe</OutputType>\n"
          (format "\n    <TargetFramework>%s</TargetFramework>" framework)
          "\n    <ImplicitUsings>enable</ImplicitUsings>"
          "\n    <Nullable>enable</Nullable>"
          (when org-babel-csharp-additional-project-flags
            (format "\n    %s" org-babel-csharp-additional-project-flags))
          "\n  </PropertyGroup>"
          "\n</Project>"))

(defun org-babel-csharp--format-usings (usings)
  "Format USINGS into a string suitable for inclusion in a C# source file.

USINGS should be a list of strings, each representing a using directive.
Returns a string with each using directive on a new line."
  (mapconcat
   (lambda (u)
     (unless (stringp u) (error "Usings must be of type string."))
     (format "using %s;" u))
   usings "\n"))

(defun org-babel-expand-body:csharp (body params)
  "Expand a block of C# code in BODY according to PARAMS.

See `org-babel-default-header-args:csharp' for available parameters."
  (let* ((main-p (not (string= (cdr (assq :main params)) "no")))
         (class (pcase (alist-get :class params)
                  ("no" nil)
                  (`nil "Program")
                  (_ (alist-get :class params))))
         (namespace "org.babel.autogen")
         (usings (alist-get :usings params)))
    (with-temp-buffer
      (when (alist-get :prologue params)
        (insert (alist-get :prologue params) "\n"))
      (insert "namespace " namespace ";\n")
      (when usings
        (insert (format "\n%s\n" (org-babel-csharp--format-usings usings))))
      (when class
        (insert "\nclass " class "\n{\n"))
      (when main-p
        (insert "static void Main(string[] args)\n{\n"))
      (insert (if (alist-get :var params)
                  (mapconcat #'identity (org-babel-variable-assignments:csharp params) "\n")
                "")
              "\n")
      (insert body)
      (when main-p
        (insert "\n}"))
      (when class
        (insert "\n}"))
      (when (alist-get :epilogue params)
        (insert "\n" (alist-get :epilogue params)))
      (buffer-string))))

(defun org-babel-csharp--format-refs (refs)
  "Format REFS into a string suitable for inclusion in a .csproj file.

REFS should be a list of strings or cons cells, each representing a reference.
If an entry is a cons cell, the car denotes the reference name and
the cdr is the version.

Returns a formatted string representing the references, categorized into
project reference, assembly reference, and package reference.
Reference types are distinguished by their file extension.
'.csproj' is interpreted as a project reference,
'.dll' as an assembly reference.
When a version is present, it will be treated as a package reference."
  (let ((projectref)
        (assemblyref)
        (systemref))
    (dolist (ref refs)
      (let* ((version (if (consp ref)
                          (cdr ref)
                        nil))
             (ref-string (if (consp ref)
                             (car ref)
                           ref))
             (full-ref (if version
                           (file-truename (car ref))
                         (file-truename ref))))
        (cond
         ((string= "csproj" (file-name-extension full-ref))
          (setf projectref
                (concat projectref
                        (format "\n    <ProjectReference Include=\"%s\" />"
                                full-ref))))
         ((string= "dll" (file-name-extension full-ref))
          (setf assemblyref
                (concat assemblyref
                        (format "\n    <Reference Include=%S>\n      <HintPath>%s</HintPath>\n    </Reference>"
                                (file-name-base full-ref) full-ref))))
         (t (setf systemref
                  (concat systemref
                          (format "\n    <PackageReference Include=%s />"
                                  (if version
                                      (format "%S Version=%S" ref-string version)
                                    (format "%S" ref-string)))))))))
    (format "%s\n\n  %s\n\n  %s"
            (if projectref
                (format "<ItemGroup>%s\n  </ItemGroup>" projectref)
              "")
            (if assemblyref
                (format "<ItemGroup>%s\n  </ItemGroup>" assemblyref)
              "")
            (if systemref
                (format "<ItemGroup>%s\n  </ItemGroup>" systemref)
              ""))))

(defun org-babel-execute:csharp (body params)
  "Execute a block of Csharp code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((full-body (org-babel-expand-body:csharp body params))
         (base-dir  (make-temp-name (file-name-concat org-babel-temporary-directory "obcs")))
         (project-name (file-name-base base-dir))
         (bin-dir (file-name-concat base-dir "bin"))
         (framework (or (alist-get :framework params) org-babel-csharp-default-target-framework))
         (program-file (file-name-concat base-dir "Program.cs"))
         (project-file (file-name-concat base-dir (concat project-name ".csproj")))
         (nuget-file (alist-get :nugetconfig params))
         (cmdline (alist-get :cmdline params))
         (cmdline (if cmdline cmdline ""))
         (restore-cmd (funcall org-babel-csharp-generate-restore-command project-file))
         (compile-cmd (funcall org-babel-csharp-generate-compile-command
                               (file-truename project-file)
                               (file-truename bin-dir)))
         (run-cmd (format "%S %S" (file-truename (file-name-concat bin-dir project-name)) cmdline)))
    (unless (org-babel-csharp--find-dotnet-version)
      (error "Could not find a .NET SDK for compiling."))
    (unless (file-exists-p base-dir)
      (make-directory base-dir))
    (with-temp-file program-file
      (insert full-body))
    (with-temp-file project-file
      (insert
       (let ((refs (alist-get :references params)))
         (org-babel-csharp--generate-project-file refs framework))))
    (when (and nuget-file (file-exists-p (file-truename nuget-file)))
      (copy-file nuget-file (file-name-concat base-dir (file-name-nondirectory (file-truename nuget-file)))))
    ;; nuget restore
    (org-babel-eval restore-cmd "")
    (let ((compile-result (org-babel-eval compile-cmd "")))
      (when (string-match ": error" compile-result)
        (org-babel-eval-error-notify 1 compile-result)))
    (let ((results (org-babel-eval run-cmd "")))
      (when results
        (setq results (org-remove-indentation results))
        ;; results
        (org-babel-reassemble-table
	 (org-babel-result-cond (cdr (assq :result-params params))
	   results
	   (let ((tmp-file (org-babel-temp-file "c-")))
	     (with-temp-file tmp-file (insert results))
	     (org-babel-import-elisp-from-file tmp-file)))
	 (org-babel-pick-name
	  (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
	 (org-babel-pick-name
	  (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

(defun org-babel-variable-assignments:csharp (params)
  "Return a list of C# variable assignments from header arguments."
  (mapcar
   #'(lambda (pair) (format "var %s = %S;" (car pair) (cdr pair)))
   (org-babel--get-vars params)))

(provide 'ob-csharp)
;;; ob-csharp.el ends here
