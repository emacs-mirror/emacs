;;; ob-ditaa.el --- Babel Functions for ditaa        -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2026 Free Software Foundation, Inc.

;; Authors: Eric Schulte, Jarmo Hurri
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

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

;; Org-Babel support for evaluating ditaa source code.
;;
;; Source code blocks of type ditaa have some special features:
;;
;; - there is no such thing as a "session"
;;
;; - :export results is the default
;;
;; - only results of type "file" are returned
;;
;; - there are no variables
;;
;; - three different variants of "ditaa" exist: a ditaa executable
;;   (shell script), ditaa.jar Java archive and DitaaEPS.jar Java
;;   archive; the third one is a fork generating eps output, and is
;;   also a prerequisite for producing pdf output; ob-ditaa supports
;;   all three of these; if ditaa.jar or DitaaEPS.jar is used, paths
;;   to file(s) must be set; the following table summarizes which
;;   variant is used in which case; column mode refers to
;;   `org-ditaa-default-exec-mode'
;;
;;   | mode           | output   | command                                             |
;;   |----------------+----------+-----------------------------------------------------|
;;   | `ditaa'        | png, svg | `org-ditaa-exec'                                    |
;;   | `jar'          | png, svg | `org-ditaa-java-exec' -jar `org-ditaa-jar-path'     |
;;   | `ditaa', `jar' | eps      | `org-ditaa-java-exec' -jar `org-ditaa-eps-jar-path' |
;;   | `ditaa', `jar' | pdf      | `org-ditaa-java-exec' -jar `org-ditaa-eps-jar-path' |
;;
;; - standard header argument "cmdline" controls command line parameters passed to ditaa
;; - the following header arguments are added:
;;   "java" : additional parameters passed to java if ditaa run via a jar
;;

;;; Requirements:

;; at least one of the following:
;;
;; ditaa (executable)
;; - packaged in some distributions
;; - configurable via `org-ditaa-exec'
;;
;; ditaa.jar | when exec mode is `jar'
;; - `org-ditaa-jar-path' must point to this jar file
;; - see https://github.com/stathissideris/ditaa
;;
;; DitaaEps.jar | when generating eps or pdf output
;; - `org-ditaa-eps-jar-path' must point to this jar file
;; - see https://sourceforge.net/projects/ditaa-addons/files/DitaaEps/

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'org-compat)

(defvar org-babel-default-header-args:ditaa
  '((:results . "file graphics")
    (:exports . "results")
    (:file-ext . "png"))
  "Default arguments for evaluating a ditaa source block.")

(defcustom org-ditaa-default-exec-mode 'jar
  "Method to use for ditaa diagram generation when generating png or svg output.
`jar' means to use java together with a JAR.
The JAR must be set via `org-ditaa-jar-path'.

`ditaa' means to use the ditaa executable.
The executable can be configured via `org-ditaa-exec'."

  :group 'org-babel
  :package-version '(Org . "9.8")
  :type '(choice (const :tag "Use java together with a JAR file." jar)
                 (const :tag "Use ditaa executable." ditaa))
  :safe (lambda (x) (memq x '(jar ditaa))))

(defcustom org-ditaa-exec "ditaa"
  "File name of the ditaa executable."
  :group 'org-babel
  :package-version '(Org . "9.8")
  :type 'string
  :risky t)

(define-obsolete-variable-alias 'org-babel-ditaa-java-cmd 'org-ditaa-java-exec "9.8")
(defcustom org-ditaa-java-exec "java"
  "Java executable to use when evaluating ditaa blocks using a JAR."
  :group 'org-babel
  :type 'string
  :risky t)

(defcustom org-ditaa-jar-path (expand-file-name
			       "ditaa.jar"
			       (file-name-as-directory
				(expand-file-name
				 "scripts"
				 (file-name-as-directory
				  (expand-file-name
				   "../contrib"
				   (file-name-directory (org-find-library-dir "org")))))))
  "Path to the ditaa.jar file."
  :group 'org-babel
  :type 'string)

(defcustom org-ditaa-eps-jar-path
  (expand-file-name "DitaaEps.jar" (file-name-directory org-ditaa-jar-path))
  "Path to the DitaaEps.jar executable.
Used when generating eps or pdf output."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(make-obsolete-variable 'org-ditaa-jar-option
                        "(ob-ditaa) variable org-ditaa-jar-option is obsolete"
                        "9.8")

(defun ob-ditaa--ensure-jar-file (file)
  "Return FILE if it exists, signal error otherwise."
  (if (file-exists-p file)
      file
    (error "(ob-ditaa) Could not find jar file %s" file)))

(defun org-babel-execute:ditaa (body params)
  "Execute BODY of ditaa code with org-babel according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (org-babel-graphical-output-file params))
         (out-file-suffix (file-name-extension out-file))
         ;; backwards-compatibility of :eps and :pdf header arguments
         ;; notice that these take precedence over file type (suffix)
         (legacy-eps (cdr (assq :eps params)))
         (legacy-pdf (cdr (assq :pdf params))))
    (when (and legacy-eps legacy-pdf)
      (error "(ob-ditaa) Both :eps and :pdf legacy output types specified"))
    (let* ((legacy-output-type (or legacy-eps legacy-pdf))
           (eps (or legacy-eps (string= out-file-suffix "eps")))
           (pdf (or legacy-pdf (string= out-file-suffix "pdf")))
           (svg (and (not legacy-output-type) (string= out-file-suffix "svg")))
           (png (and (not legacy-output-type)
                     (or (string= out-file-suffix "png")
                         (not (or svg pdf eps))))) ;; default output type is png
           (ditaa-options (cdr (assq :cmdline params)))
	   (java-options (cdr (assq :java params)))
           (use-eps-jar (or eps pdf))
           (exec-form (if (or (equal org-ditaa-default-exec-mode 'jar) use-eps-jar)
                          (concat org-ditaa-java-exec
                                  (when java-options (concat " " java-options))
                                  " "
                                  ;; use obsolete variable instead of default param if defined
                                  (if (boundp 'org-ditaa-jar-option) org-ditaa-jar-option "-jar")
                                  " "
                                  (shell-quote-argument
                                   (ob-ditaa--ensure-jar-file (if use-eps-jar org-ditaa-eps-jar-path
                                                                org-ditaa-jar-path))))
                        org-ditaa-exec))
	   (in-file (org-babel-temp-file "ditaa-"))
           (ditaa-out-file (org-babel-process-file-name (if pdf (concat in-file ".eps") out-file)))
           (ditaa-coding-system 'utf-8)
	   (cmd (concat exec-form
                        (when ditaa-options (concat " " ditaa-options))
                        (when svg (concat " " "--svg"))
                        " " "-e" " " (symbol-name ditaa-coding-system)
                        " " in-file " " ditaa-out-file)))
      ;; verify that output file type is specified - note that this
      ;; error should in fact never happen, since default png type is
      ;; set above if no other supported type is specified
      (unless (or eps pdf svg png)
        (error (concat "(ob-ditaa) Unknown output file extension: " out-file-suffix)))
      (with-temp-file in-file
        (set-buffer-file-coding-system ditaa-coding-system)
        (insert body))
      (shell-command cmd)
      (when pdf
        (shell-command (concat "epstopdf" " " ditaa-out-file " "
		               "-o=" (org-babel-process-file-name out-file))))
      nil))) ;; signal that output has already been written to file

(defun org-babel-prep-session:ditaa (_session _params)
  "Return an error because ditaa does not support sessions."
  (error "Ditaa does not support sessions"))

(provide 'ob-ditaa)

;;; ob-ditaa.el ends here
