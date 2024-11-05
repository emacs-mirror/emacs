;;; ob-lilypond.el --- Babel Functions for Lilypond  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2024 Free Software Foundation, Inc.

;; Author: Martyn Jago
;; Keywords: babel language, literate programming
;; URL: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lilypond.html

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

;; Installation, ob-lilypond documentation, and examples are available at
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lilypond.html
;;
;; Lilypond documentation can be found at
;; https://lilypond.org/manuals.html
;;
;; This depends on epstopdf --- See https://www.ctan.org/pkg/epstopdf.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)

(declare-function org-fold-show-all "org-fold" (&optional types))

(add-to-list 'org-babel-tangle-lang-exts '("LilyPond" . "ly"))
(add-to-list 'org-src-lang-modes '("lilypond" . LilyPond))

(defvar org-babel-default-header-args:lilypond '()
  "Default header arguments for lilypond code blocks.
NOTE: The arguments are determined at lilypond compile time.
See `org-babel-lilypond-set-header-args'
To configure, see `ob-lilypond-header-args'
.")

(defvar ob-lilypond-header-args
  '((:results . "file") (:exports . "results"))
  "User-configurable header arguments for lilypond code blocks.
NOTE: The final value used by org-babel is computed at compile-time
and stored in  `org-babel-default-header-args:lilypond'
See `org-babel-lilypond-set-header-args'.")

(defvar org-babel-lilypond-compile-post-tangle t
  "When non-nil, compile tangled file after `org-babel-tangle'.")

(defvar org-babel-lilypond-display-pdf-post-tangle t
  "When non-nil, display pdf after successful LilyPond compilation.")

(defvar org-babel-lilypond-play-midi-post-tangle t
  "When non-nil, play midi file after successful LilyPond compilation.")

(defvar org-babel-lilypond-ly-command ""
  "Command to execute lilypond on your system.
Do not set it directly.  Customize `org-babel-lilypond-commands' instead.")

(defvar org-babel-lilypond-pdf-command ""
  "Command to show a PDF file on your system.
Do not set it directly.  Customize `org-babel-lilypond-commands' instead.")

(defvar org-babel-lilypond-midi-command ""
  "Command to play a MIDI file on your system.
Do not set it directly.  Customize `org-babel-lilypond-commands' instead.")

(defcustom org-babel-lilypond-commands
  (cond
   ((eq system-type 'darwin)
    '("/Applications/lilypond.app/Contents/Resources/bin/lilypond" "open" "open"))
   ((eq system-type 'windows-nt)
    '("lilypond" "" ""))
   (t
    '("lilypond" "xdg-open" "xdg-open")))
  "Commands to run lilypond and view or play the results.
These should be executables that take a filename as an argument.
On some system it is possible to specify the filename directly
and the viewer or player will be determined from the file type;
you can leave the string empty on this case."
  :group 'org-babel
  :type '(list
	  (string :tag "Lilypond   ")
	  (string :tag "PDF Viewer ")
	  (string :tag "MIDI Player"))
  :version "24.4"
  :package-version '(Org . "8.2.7")
  :set
  (lambda (symbol value)
    (set-default-toplevel-value symbol value)
    (setq
     org-babel-lilypond-ly-command   (nth 0 value)
     org-babel-lilypond-pdf-command  (nth 1 value)
     org-babel-lilypond-midi-command (nth 2 value))))

(defvar org-babel-lilypond-gen-png nil
  "Non-nil means image generation (PNG) is turned on by default.")

(defvar org-babel-lilypond-gen-svg nil
  "Non-nil means image generation (SVG) is be turned on by default.")

(defvar org-babel-lilypond-gen-html nil
  "Non-nil means HTML generation is turned on by default.")

(defvar org-babel-lilypond-gen-pdf nil
  "Non-nil means PDF generation is be turned on by default.")

(defvar org-babel-lilypond-use-eps nil
  "Non-nil forces the compiler to use the EPS backend.")

(defvar org-babel-lilypond-arrange-mode nil
  "Non-nil turns Arrange mode on.
In Arrange mode the following settings are altered from default:
:tangle yes,    :noweb yes
:results silent :comments yes.
In addition lilypond block execution causes tangling of all lilypond
blocks.")

(defun org-babel-expand-body:lilypond (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
        (prologue (cdr (assq :prologue params)))
        (epilogue (cdr (assq :epilogue params))))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body t t))))
     vars)
    (concat
     (and prologue (concat prologue "\n"))
     body
     (and epilogue (concat "\n" epilogue "\n")))))

(defun org-babel-execute:lilypond (body params)
  "Execute LilyPond src block according to arrange mode.
See `org-babel-execute-src-block' for BODY and PARAMS.
When in arrange mode, tangle all blocks and process the result.
Otherwise, execute block according to header settings."
  (org-babel-lilypond-set-header-args org-babel-lilypond-arrange-mode)
  (if org-babel-lilypond-arrange-mode
      (org-babel-lilypond-tangle)
    (org-babel-lilypond-process-basic body params)))

(defun org-babel-lilypond-tangle ()
  "Tangle lilypond blocks, then `org-babel-lilypond-execute-tangled-ly'."
  (interactive)
  (if (org-babel-tangle nil "yes" "lilypond")
      (org-babel-lilypond-execute-tangled-ly) nil))

;; https://lilypond.org/doc/v2.24/Documentation/usage/other-programs
(defvar org-babel-lilypond-paper-settings
  "#(if (ly:get-option 'use-paper-size-for-page)
            (begin (ly:set-option 'use-paper-size-for-page #f)
                   (ly:set-option 'tall-page-formats '%s)))
\\paper {
  indent=0\\mm
  tagline=\"\"
  oddFooterMarkup=##f
  oddHeaderMarkup=##f
  bookTitleMarkup=##f
  scoreTitleMarkup=##f
}\n"
  "The paper settings required to generate music fragments.
They are needed for mixing music and text in basic-mode.")

(defun org-babel-lilypond-process-basic (body params)
  "Execute a lilypond block in basic mode.
See `org-babel-execute-src-block' for BODY and PARAMS."
  (let* ((out-file (cdr (assq :file params)))
         (file-type (file-name-extension out-file))
	 (cmdline (or (cdr (assq :cmdline params))
		      ""))
	 (in-file (org-babel-temp-file "lilypond-")))

    (with-temp-file in-file
      (insert
       (format org-babel-lilypond-paper-settings file-type)
       (org-babel-expand-body:generic body params)))
    (org-babel-eval
     (concat
      org-babel-lilypond-ly-command
      " -dbackend=eps "
      "-dno-gs-load-fonts "
      "-dinclude-eps-fonts "
      (or (assoc-default file-type
                         '(("pdf" . "--pdf ")
			   ("eps" . "--eps ")))
	  "--png ")
      "--output="
      (file-name-sans-extension out-file)
      " "
      cmdline
      in-file)
     ""))
  nil)

(defun org-babel-prep-session:lilypond (_session _params)
  "Return an error because LilyPond exporter does not support sessions."
  (error "Sorry, LilyPond does not currently support sessions!"))

(defun org-babel-lilypond-execute-tangled-ly ()
  "Compile result of block tangle with lilypond.
If error in compilation, attempt to mark the error in lilypond org file."
  (when org-babel-lilypond-compile-post-tangle
    (let ((org-babel-lilypond-tangled-file (org-babel-lilypond-switch-extension
                                            (buffer-file-name) ".lilypond"))
          (org-babel-lilypond-temp-file (org-babel-lilypond-switch-extension
                                         (buffer-file-name) ".ly")))
      (if (not (file-exists-p org-babel-lilypond-tangled-file))
	  (error "Error: Tangle Failed!")
	(when (file-exists-p org-babel-lilypond-temp-file)
	  (delete-file org-babel-lilypond-temp-file))
	(rename-file org-babel-lilypond-tangled-file
		     org-babel-lilypond-temp-file))
      (switch-to-buffer-other-window "*lilypond*")
      (erase-buffer)
      (org-babel-lilypond-compile-lilyfile org-babel-lilypond-temp-file)
      (goto-char (point-min))
      (if (org-babel-lilypond-check-for-compile-error org-babel-lilypond-temp-file)
	  (error "Error in Compilation!")
	(other-window -1)
	(org-babel-lilypond-attempt-to-open-pdf org-babel-lilypond-temp-file)
	(org-babel-lilypond-attempt-to-play-midi org-babel-lilypond-temp-file)))))

;;Ignoring second arg for pre Org 9.7 compatibility
(defun org-babel-lilypond-compile-lilyfile (filename &optional _)
  "Compile Lilypond FILENAME and check for compile errors."
  (message "Compiling %s..." filename)
  (let ((args (delq nil (list
                         (and org-babel-lilypond-gen-png  "--png")
                         (and org-babel-lilypond-gen-html "--html")
                         (and org-babel-lilypond-gen-pdf  "--pdf")
                         (and org-babel-lilypond-use-eps  "-dbackend=eps")
                         (and org-babel-lilypond-gen-svg  "-dbackend=svg")
                         (concat "--output=" (file-name-sans-extension filename))
                         filename))))
    (apply #'call-process org-babel-lilypond-ly-command nil
           "*lilypond*" 'display args)))

(defun org-babel-lilypond-check-for-compile-error (file-name &optional test)
  "Check for compile error.
This is performed by parsing the *lilypond* buffer
containing the output message from the compilation.
FILE-NAME is full path to lilypond file.
If TEST is t just return nil if no error found, and pass
nil as file-name since it is unused in this context."
  (let ((is-error (search-forward "error:" nil t)))
    (if test
	is-error
      (when is-error
	(org-babel-lilypond-process-compile-error file-name)))))

(defun org-babel-lilypond-process-compile-error (file-name)
  "Process the compilation error that has occurred.
FILE-NAME is full path to lilypond file."
  (let ((line-num (org-babel-lilypond-parse-line-num)))
    (let ((error-lines (org-babel-lilypond-parse-error-line file-name line-num)))
      (org-babel-lilypond-mark-error-line file-name error-lines)
      (error "Error: Compilation Failed!"))))

(defun org-babel-lilypond-mark-error-line (file-name line)
  "Mark the erroneous lines in the lilypond org buffer.
FILE-NAME is full path to lilypond file.
LINE is the erroneous line."
  (switch-to-buffer-other-window
   (concat (file-name-nondirectory
            (org-babel-lilypond-switch-extension file-name ".org"))))
  (let ((temp (point)))
    (goto-char (point-min))
    (setq case-fold-search nil)
    (if (search-forward line nil t)
        (progn
          (org-fold-show-all)
          (set-mark (point))
          (goto-char (- (point) (length line))))
      (goto-char temp))))

(defun org-babel-lilypond-parse-line-num (&optional buffer)
  "Extract error line number in BUFFER or `current-buffer'."
  (when buffer (set-buffer buffer))
  (let ((start
         (and (search-backward ":" nil t)
              (search-backward ":" nil t)
              (search-backward ":" nil t)
              (search-backward ":" nil t))))
    (when start
      (forward-char)
      (let ((num (string-to-number
		  (buffer-substring
		   (+ 1 start)
		   (- (search-forward ":" nil t) 1)))))
	(and (numberp num) num)))))

(defun org-babel-lilypond-parse-error-line (file-name lineNo)
  "Extract the erroneous line from the tangled .ly file.
FILE-NAME is full path to lilypond file.
LINENO is the number of the erroneous line."
  (with-temp-buffer
    (insert-file-contents (org-babel-lilypond-switch-extension file-name ".ly")
			  nil nil nil t)
    (if (> lineNo 0)
	(progn
	  (goto-char (point-min))
	  (forward-line (- lineNo 1))
          (buffer-substring (point) (line-end-position)))
      nil)))

(defun org-babel-lilypond-attempt-to-open-pdf (file-name &optional test)
  "Attempt to display the generated pdf file.
FILE-NAME is full path to lilypond file.
If TEST is non-nil, the shell command is returned and is not run."
  (when org-babel-lilypond-display-pdf-post-tangle
    (let ((pdf-file (org-babel-lilypond-switch-extension file-name ".pdf")))
      (if (file-exists-p pdf-file)
          (let ((cmd-string
                 (concat org-babel-lilypond-pdf-command " " pdf-file)))
            (if test
                cmd-string
	      (start-process
	       "\"Audition pdf\""
	       "*lilypond*"
	       org-babel-lilypond-pdf-command
	       pdf-file)))
	(message  "No pdf file generated so can't display!")))))

(defun org-babel-lilypond-attempt-to-play-midi (file-name &optional test)
  "Attempt to play the generated MIDI file.
FILE-NAME is full path to lilypond file.
If TEST is non-nil, the shell command is returned and is not run."
  (when org-babel-lilypond-play-midi-post-tangle
    (let* ((ext (if (eq system-type 'windows-nt)
                    ".mid" ".midi"))
           (midi-file (org-babel-lilypond-switch-extension file-name ext)))
      (if (file-exists-p midi-file)
          (let ((cmd-string
                 (concat org-babel-lilypond-midi-command " " midi-file)))
            (if test
                cmd-string
              (start-process
               "\"Audition midi\""
               "*lilypond*"
               org-babel-lilypond-midi-command
               midi-file)))
        (message "No midi file generated so can't play!")))))

(defun org-babel-lilypond-toggle-midi-play ()
  "Toggle whether midi will be played following a successful compilation."
  (interactive)
  (setq org-babel-lilypond-play-midi-post-tangle
        (not org-babel-lilypond-play-midi-post-tangle))
  (message (concat "Post-Tangle MIDI play has been "
                   (if org-babel-lilypond-play-midi-post-tangle
                       "ENABLED." "DISABLED."))))

(defun org-babel-lilypond-toggle-pdf-display ()
  "Toggle whether pdf will be displayed following a successful compilation."
  (interactive)
  (setq org-babel-lilypond-display-pdf-post-tangle
        (not org-babel-lilypond-display-pdf-post-tangle))
  (message (concat "Post-Tangle PDF display has been "
                   (if org-babel-lilypond-display-pdf-post-tangle
                       "ENABLED." "DISABLED."))))

(defun org-babel-lilypond-toggle-png-generation ()
  "Toggle whether png image will be generated by compilation."
  (interactive)
  (setq org-babel-lilypond-gen-png (not org-babel-lilypond-gen-png))
  (message (concat "PNG image generation has been "
                   (if org-babel-lilypond-gen-png "ENABLED." "DISABLED."))))

(defun org-babel-lilypond-toggle-html-generation ()
  "Toggle whether html will be generated by compilation."
  (interactive)
  (setq org-babel-lilypond-gen-html (not org-babel-lilypond-gen-html))
  (message (concat "HTML generation has been "
                   (if org-babel-lilypond-gen-html "ENABLED." "DISABLED."))))

(defun org-babel-lilypond-toggle-pdf-generation ()
  "Toggle whether pdf will be generated by compilation."
  (interactive)
  (setq org-babel-lilypond-gen-pdf (not org-babel-lilypond-gen-pdf))
  (message (concat "PDF generation has been "
                   (if org-babel-lilypond-gen-pdf "ENABLED." "DISABLED."))))

(defun org-babel-lilypond-toggle-arrange-mode ()
  "Toggle whether in Arrange mode or Basic mode."
  (interactive)
  (setq org-babel-lilypond-arrange-mode
        (not org-babel-lilypond-arrange-mode))
  (message (concat "Arrange mode has been "
                   (if org-babel-lilypond-arrange-mode "ENABLED." "DISABLED."))))

(defun org-babel-lilypond-switch-extension (file-name ext)
  "Utility command to swap current FILE-NAME extension with EXT."
  (concat (file-name-sans-extension
           file-name)
	  ext))

(defun org-babel-lilypond-get-header-args (mode)
  "Default arguments to use when evaluating a lilypond source block.
These depend upon whether we are in Arrange mode i.e. MODE is t."
  (cond (mode
         '((:tangle . "yes")
           (:noweb . "yes")
           (:results . "silent")
           (:cache . "yes")
           (:comments . "yes")))
        (t
         ob-lilypond-header-args)))

(defun org-babel-lilypond-set-header-args (mode)
  "Set lilypond babel header according to MODE."
  (setq org-babel-default-header-args:lilypond
        (org-babel-lilypond-get-header-args mode)))

(provide 'ob-lilypond)

;;; ob-lilypond.el ends here
