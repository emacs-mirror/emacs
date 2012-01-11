;;; emacsbug.el --- command to report Emacs bugs to appropriate mailing list

;; Copyright (C) 1985, 1994, 1997, 1998, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
;;   Free Software Foundation, Inc.

;; Author: K. Shane Hartman
;; Maintainer: FSF
;; Keywords: maint mail

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `M-x report-emacs-bug' starts an email note to the Emacs maintainers
;; describing a problem.  You need to be able to send mail from Emacs
;; to complete the process.  Alternatively, compose the bug report in
;; Emacs then paste it into your normal mail client.

;;; Code:

(defgroup emacsbug nil
  "Sending Emacs bug reports."
  :group 'maint
  :group 'mail)

(defcustom report-emacs-bug-address "bug-gnu-emacs@gnu.org"
  "Address of mailing list for GNU Emacs bugs."
  :group 'emacsbug
  :type 'string)

(defcustom report-emacs-bug-pretest-address "bug-gnu-emacs@gnu.org"
  "Address of mailing list for GNU Emacs pretest bugs."
  :group 'emacsbug
  :type 'string
  :version "23.2")                ; emacs-pretest-bug -> bug-gnu-emacs

(defcustom report-emacs-bug-no-confirmation nil
  "If non-nil, suppress the confirmations asked for the sake of novice users."
  :group 'emacsbug
  :type 'boolean)

(defcustom report-emacs-bug-no-explanations nil
  "If non-nil, suppress the explanations given for the sake of novice users."
  :group 'emacsbug
  :type 'boolean)

;; User options end here.


(defvar report-emacs-bug-orig-text nil
  "The automatically-created initial text of the bug report.")

(defvar report-emacs-bug-send-command nil
  "Name of the command to send the bug report, as a string.")
(make-variable-buffer-local 'report-emacs-bug-send-command)

(defvar report-emacs-bug-send-hook nil
  "Hook run before sending the bug report.")
(make-variable-buffer-local 'report-emacs-bug-send-hook)

(declare-function x-server-vendor "xfns.c" (&optional terminal))
(declare-function x-server-version "xfns.c" (&optional terminal))
(declare-function message-sort-headers "message" ())

;;;###autoload
(defun report-emacs-bug (topic &optional recent-keys)
  "Report a bug in GNU Emacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  ;; This strange form ensures that (recent-keys) is the value before
  ;; the bug subject string is read.
  (interactive (reverse (list (recent-keys) (read-string "Bug Subject: "))))
  ;; The syntax `version;' is preferred to `[version]' because the
  ;; latter could be mistakenly stripped by mailing software.
  (if (eq system-type 'ms-dos)
      (setq topic (concat emacs-version "; " topic))
    (when (string-match "^\\(\\([.0-9]+\\)*\\)\\.[0-9]+$" emacs-version)
      (setq topic (concat (match-string 1 emacs-version) "; " topic))))
  ;; If there are four numbers in emacs-version (three for MS-DOS),
  ;; this is a pretest version.
  (let* ((pretest-p (string-match (if (eq system-type 'ms-dos)
				      "\\..*\\."
				    "\\..*\\..*\\.")
				  emacs-version))
	 (from-buffer (current-buffer))
	 (reporting-address (if pretest-p
				report-emacs-bug-pretest-address
			      report-emacs-bug-address))
	 ;; Put these properties on semantically-void text.
	 ;; report-emacs-bug-hook deletes these regions before sending.
	 (prompt-properties '(field emacsbug-prompt
				    intangible but-helpful
				    rear-nonsticky t))
	 user-point message-end-point)
    (setq message-end-point
	  (with-current-buffer (get-buffer-create "*Messages*")
	    (point-max-marker)))
    (compose-mail reporting-address topic)
    ;; The rest of this does not execute if the user was asked to
    ;; confirm and said no.
    ;; Message-mode sorts the headers before sending.  We sort now so
    ;; that report-emacs-bug-orig-text remains valid.  (Bug#5178)
    (if (eq major-mode 'message-mode)
        (message-sort-headers))
    (rfc822-goto-eoh)
    (forward-line 1)
    (let ((signature (buffer-substring (point) (point-max))))
      (delete-region (point) (point-max))
      (insert signature)
      (backward-char (length signature)))
    (unless report-emacs-bug-no-explanations
      ;; Insert warnings for novice users.
      (when (string-match "@gnu\\.org$" reporting-address)
	(insert "This bug report will be sent to the Free Software Foundation,\n")
	(let ((pos (point)))
	  (insert "not to your local site managers!")
          (overlay-put (make-overlay pos (point)) 'face 'highlight)))
      (insert "\nPlease write in ")
      (let ((pos (point)))
	(insert "English")
        (overlay-put (make-overlay pos (point)) 'face 'highlight))
      (insert " if possible, because the Emacs maintainers
usually do not have translators to read other languages for them.\n\n")
      (insert (format "Your report will be posted to the %s mailing list"
		      reporting-address))
      ;; Nowadays all bug reports end up there.
;;;      (if pretest-p (insert ".\n\n")
	(insert "\nand the gnu.emacs.bug news group, and at http://debbugs.gnu.org.\n\n"))

    (insert "Please describe exactly what actions triggered the bug\n"
	    "and the precise symptoms of the bug.  If you can, give\n"
	    "a recipe starting from `emacs -Q':\n\n")
    ;; Stop message-mode stealing the properties we are about to add.
    (if (boundp 'message-strip-special-text-properties)
        (set (make-local-variable 'message-strip-special-text-properties) nil))
    (add-text-properties (save-excursion
                           (rfc822-goto-eoh)
                           (line-beginning-position 2))
                         (point)
                         prompt-properties)
    (setq user-point (point))
    (insert "\n\n")

    (insert "If Emacs crashed, and you have the Emacs process in the gdb debugger,\n"
	    "please include the output from the following gdb commands:\n"
	    "    `bt full' and `xbacktrace'.\n")

    (let ((debug-file (expand-file-name "DEBUG" data-directory)))
      (if (file-readable-p debug-file)
	  (insert "For information about debugging Emacs, please read the file\n"
		  debug-file ".\n")))
    (add-text-properties (1+ user-point) (point) prompt-properties)

    (insert "\n\nIn " (emacs-version) "\n")
    (if (fboundp 'x-server-vendor)
	(condition-case nil
            ;; This is used not only for X11 but also W32 and others.
	    (insert "Windowing system distributor `" (x-server-vendor)
                    "', version "
		    (mapconcat 'number-to-string (x-server-version) ".") "\n")
	  (error t)))
    (if (and system-configuration-options
	     (not (equal system-configuration-options "")))
	(insert "configured using `configure "
		system-configuration-options "'\n\n"))
    (insert "Important settings:\n")
    (mapc
     '(lambda (var)
	(insert (format "  value of $%s: %s\n" var (getenv var))))
     '("LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES"
       "LC_MONETARY" "LC_NUMERIC" "LC_TIME" "LANG" "XMODIFIERS"))
    (insert (format "  locale-coding-system: %s\n" locale-coding-system))
    (insert (format "  default enable-multibyte-characters: %s\n"
		    (default-value 'enable-multibyte-characters)))
    (insert "\n")
    (insert (format "Major mode: %s\n"
		    (format-mode-line
                     (buffer-local-value 'mode-name from-buffer)
                     nil nil from-buffer)))
    (insert "\n")
    (insert "Minor modes in effect:\n")
    (dolist (mode minor-mode-list)
      (and (boundp mode) (buffer-local-value mode from-buffer)
	   (insert (format "  %s: %s\n" mode
			   (buffer-local-value mode from-buffer)))))
    (insert "\n")
    (insert "Recent input:\n")
    (let ((before-keys (point)))
      (insert (mapconcat (lambda (key)
			   (if (or (integerp key)
				   (symbolp key)
				   (listp key))
			       (single-key-description key)
			     (prin1-to-string key nil)))
			 (or recent-keys (recent-keys))
			 " "))
      (save-restriction
	(narrow-to-region before-keys (point))
	(goto-char before-keys)
	(while (progn (move-to-column 50) (not (eobp)))
	  (search-forward " " nil t)
	  (insert "\n"))))
    (let ((message-buf (get-buffer "*Messages*")))
      (if message-buf
	  (let (beg-pos
		(end-pos message-end-point))
	    (with-current-buffer message-buf
	      (goto-char end-pos)
	      (forward-line -10)
	      (setq beg-pos (point)))
	    (insert "\n\nRecent messages:\n")
	    (insert-buffer-substring message-buf beg-pos end-pos))))
    ;; After Recent messages, to avoid the messages produced by
    ;; list-load-path-shadows.
    (unless (looking-back "\n")
      (insert "\n"))
    (insert "\n")
    (insert "Load-path shadows:\n")
    (message "Checking for load-path shadows...")
    (let ((shadows (list-load-path-shadows t)))
      (message "Checking for load-path shadows...done")
      (insert (if (zerop (length shadows))
                  "None found.\n"
                shadows)))
    (insert (format "\nFeatures:\n%s\n" features))
    (fill-region (line-beginning-position 0) (point))
    ;; This is so the user has to type something in order to send easily.
    (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "\C-c\C-i" 'report-emacs-bug-info)
    ;; Could test major-mode instead.
    (cond ((memq mail-user-agent '(message-user-agent gnus-user-agent))
           (setq report-emacs-bug-send-command "message-send-and-exit"
                 report-emacs-bug-send-hook 'message-send-hook))
          ((eq mail-user-agent 'sendmail-user-agent)
           (setq report-emacs-bug-send-command "mail-send-and-exit"
                 report-emacs-bug-send-hook 'mail-send-hook))
          ((eq mail-user-agent 'mh-e-user-agent)
           (setq report-emacs-bug-send-command "mh-send-letter"
                 report-emacs-bug-send-hook 'mh-before-send-letter-hook)))
    (unless report-emacs-bug-no-explanations
      (with-output-to-temp-buffer "*Bug Help*"
	(princ "While in the mail buffer:\n\n")
        (if report-emacs-bug-send-command
            (princ (substitute-command-keys
                    (format "  Type \\[%s] to send the bug report.\n"
                            report-emacs-bug-send-command))))
	(princ (substitute-command-keys
		"  Type \\[kill-buffer] RET to cancel (don't send it).\n"))
	(terpri)
	(princ (substitute-command-keys
		"  Type \\[report-emacs-bug-info] to visit in Info the Emacs Manual section
    about when and how to write a bug report, and what
    information you should include to help fix the bug.")))
      (shrink-window-if-larger-than-buffer (get-buffer-window "*Bug Help*")))
    ;; Make it less likely people will send empty messages.
    (if report-emacs-bug-send-hook
        (add-hook report-emacs-bug-send-hook 'report-emacs-bug-hook nil t))
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (make-local-variable 'report-emacs-bug-orig-text)
    (setq report-emacs-bug-orig-text
          (buffer-substring-no-properties (point-min) (point)))
    (goto-char user-point)))

(defun report-emacs-bug-info ()
  "Go to the Info node on reporting Emacs bugs."
  (interactive)
  (info "(emacs)Bugs"))

(defun report-emacs-bug-hook ()
  "Do some checking before sending a bug report."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (and (= (- (point) (point-min))
            (length report-emacs-bug-orig-text))
         (string-equal (buffer-substring-no-properties (point-min) (point))
                       report-emacs-bug-orig-text)
         (error "No text entered in bug report"))
    ;; Check the buffer contents and reject non-English letters.
    ;; FIXME message-mode probably does this anyway.
    (goto-char (point-min))
    (skip-chars-forward "\0-\177")
    (unless (eobp)
      (if (or report-emacs-bug-no-confirmation
              (y-or-n-p "Convert non-ASCII letters to hexadecimal? "))
          (while (progn (skip-chars-forward "\0-\177")
                        (not (eobp)))
            (let ((ch (following-char)))
              (delete-char 1)
              (insert (format "=%02x" ch))))))

    ;; The last warning for novice users.
    (unless (or report-emacs-bug-no-confirmation
                (yes-or-no-p
                 "Send this bug report to the Emacs maintainers? "))
      (goto-char (point-min))
      (if (search-forward "To: ")
          (delete-region (point) (line-end-position)))
      (if report-emacs-bug-send-hook
          (kill-local-variable report-emacs-bug-send-hook))
      (with-output-to-temp-buffer "*Bug Help*"
	(princ (substitute-command-keys
                (format "\
You invoked the command M-x report-emacs-bug,
but you decided not to mail the bug report to the Emacs maintainers.

If you want to mail it to someone else instead,
please insert the proper e-mail address after \"To: \",
and send the mail again%s."
                        (if report-emacs-bug-send-command
                            (format " using \\[%s]"
                                    report-emacs-bug-send-command)
                          "")))))
      (error "M-x report-emacs-bug was cancelled, please read *Bug Help* buffer"))

    ;; Delete the uninteresting text that was just to help fill out the report.
    (rfc822-goto-eoh)
    (forward-line 1)
    (let ((pos (1- (point))))
      (while (setq pos (text-property-any pos (point-max)
                                          'field 'emacsbug-prompt))
        (delete-region pos (field-end (1+ pos)))))))

(provide 'emacsbug)

;;; emacsbug.el ends here
