;;; icomplete.el - minibuffer completion incremental feedback
;;; This package is in the public domain.

;;; Author: Ken Manheimer <klm@nist.gov>
;;; Maintainer: Ken Manheimer <klm@nist.gov>
;;; Version: icomplete.el,v 3.3 1993/12/11 11:27:35 klm Exp klm
;;; Created: Mar 1993 klm@nist.gov - first release to usenet
;;; Keywords: help, abbrev

;;; Commentary:

;;; Loading this package implements a more finely-grained completion
;;; feedback scheme, indicating, within the minibuffer, the
;;; prospective minibuffer completion candidates, as you type.  See
;;; the documentation string for 'icomplete-prompt' for a specific
;;; description of icompletion.

;;; It should run on most version of Emacs 19 (including Lucid emacs
;;; 19 - thanks to the efforts of Colin Rafferty (craffert@lehman.com)
;;; - thanks, Colin!)  This version of icomplete will *not* work on
;;; Emacs 18 versions - the elisp archives, at
;;; archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive, probably
;;; still has a version that works in GNU Emacs v18.

;;; Thanks to Colin Rafferty for assistance reconciling for lemacs,
;;; and to Michael Cook, who implemented an incremental completion
;;; style in his 'iswitch' functions that served as the basis for
;;; icomplete.

;;; Code:

;;;_* (Allout outline root topic.  Please leave this in.)

;;;_ + Provide
(provide 'icomplete)

;;;_ + User Customization variables
;;;_  = icomplete-inhibit
(defvar icomplete-inhibit nil
  "*Set this variable to t at any time to inhibit icomplete.")

;;;_ + Internal Variables
;;;_  = icomplete-eoinput 1
(defvar icomplete-eoinput 1
  "Point where minibuffer input ends and completion info begins.")
(make-variable-buffer-local 'icomplete-eoinput)

;;;_ > icomplete-prime-session ()
;;;###autoload
(defun icomplete-prime-session ()

  "Prep emacs v 19 for more finely-grained minibuffer completion-feedback.

You can inhibit icomplete after loading by setting icomplete-inhibit
non-nil.  Set the var back to nil to re-enable icomplete."

  ;; For emacs v19.18 and later revs, the icomplete key function is
  ;; installed in 'minibuffer-setup-hook'.  Global pre- and post-
  ;; command-hook functions are used in v19.17 and earlier v19 revs."

  (let* ((v19-rev (and (string-match "^19\\.\\([0-9]+\\)" emacs-version)
		       (string-to-int (substring emacs-version
						 (match-beginning 1)
						 (match-end 1))))))

    (cond ((and v19-rev			; emacs v 19, some rev,
		(> v19-rev 17))
	   ;; Post v19rev17, has minibuffer-setup-hook, use it:
	   (add-hook 'minibuffer-setup-hook 'icomplete-prime-minibuffer))
	  (v19-rev
	   ;; v19rev17 and prior (including lucid): use global
	   ;; pre- and post-command-hooks, instead:
	   (add-hook 'pre-command-hook 'icomplete-pre-command-hook 'append)
	   (add-hook 'post-command-hook
		     'icomplete-post-command-hook 'append))
	  ((format "icomplete: non v19 emacs, %s - %s"
		   emacs-version "try elisp-archive icomplete")))))

;;;_ > icomplete-prime-minibuffer ()
(defun icomplete-prime-minibuffer ()
  "Prep emacs, v 19.18 or later, for icomplete.
\(In emacs v19.17 and earlier, and in lemacs, icomplete-prime-session
is used, instead to establish global hooks.\)

Run via `minibuffer-setup-hook', adds icomplete pre- and post-command
hooks at the start of each minibuffer."

  ;; Append the hooks to avoid as much as posssible interference from
  ;; other hooks that foul up minibuffer quit.
  (make-local-variable 'pre-command-hook)
  (make-local-variable 'post-command-hook)
  (setq pre-command-hook (copy-sequence pre-command-hook))
  (setq post-command-hook (copy-sequence post-command-hook))
  (add-hook 'pre-command-hook 'icomplete-pre-command-hook)
  (add-hook 'post-command-hook 'icomplete-post-command-hook))

;;;_ > icomplete-window-minibuffer-p ()
(defmacro icomplete-window-minibuffer-p ()

  "Returns non-nil if current window is a minibuffer window.

Trivially equates to '(window-minibuffer-p (selected-window))', with
the argument definitely provided for emacsen that require it, eg Lucid."

  '(window-minibuffer-p (selected-window)))

;;;_ + Completion

;;;_  - Completion feedback hooks
;;;_   > icomplete-pre-command-hook ()
(defun icomplete-pre-command-hook ()
  "Cleanup completions display before user's new command is dealt with."
  (if (and (icomplete-window-minibuffer-p)
	   (not executing-macro)
	   (not (symbolp minibuffer-completion-table))
	   (not icomplete-inhibit))
      (if (and (boundp 'icomplete-eoinput)
	       icomplete-eoinput)
	  (if (> icomplete-eoinput (point-max))
	      ;; Oops, got rug pulled out from under us - reinit:
	      (setq icomplete-eoinput (point-max))
	    (let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	      (delete-region icomplete-eoinput (point-max))))
	;; Reestablish the local variable 'cause minibuffer-setup is weird:
	(make-local-variable 'icomplete-eoinput)
	(setq icomplete-eoinput 1))))
;;;_   > icomplete-post-command-hook ()
(defun icomplete-post-command-hook ()
  "Exhibit completions, leaving bookkeeping so pre- hook can tidy up."

  (if (and (icomplete-window-minibuffer-p)	; ... in a minibuffer.
	   (not executing-macro)
	   (not icomplete-inhibit)	; ... not specifically inhibited.
	   ;(sit-for 0)			; ... redisplay and if there's input
					; waiting, then don't icomplete
					; (stigs suggestion) (too jumpy!)
	   ;; Inhibit for file-name and other custom-func completions:
	   (not (symbolp minibuffer-completion-table))
	   )
      (let ((buffer-undo-list buffer-undo-list ))	; prevent entry
	(icomplete-exhibit))))
;;;_   > icomplete-window-setup-hook ()
(defun icomplete-window-setup-hook ()
  "Exhibit completions, leaving bookkeeping so pre- hook can tidy up."

  (if (and (icomplete-window-minibuffer-p)	; ... in a minibuffer.
	   )
      (message "ic ws doing")(sit-for 1)))
;;;_   > icomplete-exhibit ()
(defun icomplete-exhibit ()
  "Insert icomplete completions display."
  (if (not (symbolp minibuffer-completion-table))
      (let ((contents (buffer-substring (point-min)(point-max)))
	    (buffer-undo-list t))
	(save-excursion
	  (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
	  (if (not (boundp 'icomplete-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'icomplete-eoinput))
	  (setq icomplete-eoinput (point))
                                        ; Insert the match-status information:
	  (if (> (point-max) 1)
	      (insert-string
		(icomplete-prompt contents
				  minibuffer-completion-table
				  minibuffer-completion-predicate
				  (not
				   minibuffer-completion-confirm))))))))

;;;_  - Completion feedback producer
;;;_   > icomplete-prompt (name candidates predicate require-match)
(defun icomplete-prompt (name candidates predicate require-match)
  "Identify prospective candidates for minibuffer completion.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
one of \(), \[], or \{} pairs.  The choice of brackets is as follows:

  \(...) - a single prospect is identified and matching is enforced,
  \[...] - a single prospect is identified but matching is optional, or
  \{...} - multiple prospects, separated by commas, are indicated, and
          further input is required to distingish a single one.

The displays for disambiguous matches have \" [Matched]\" appended
\(whether complete or not), or \" \[No matches]\", if no eligible
matches exist."

  (let ((comps (all-completions name candidates predicate))
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]"))
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
        )
    (cond ((null comps) (format " %sNo matches%s"
                                open-bracket-determined
                                close-bracket-determined))
          ((null (cdr comps))           ;one match
           (concat (if (and (> (length (car comps))
                               (length name)))
                       (concat open-bracket-determined
                               (substring (car comps) (length name))
                               close-bracket-determined)
                     "")
                   " [Matched]"))
          (t                            ;multiple matches
           (let* ((most (try-completion name candidates predicate))
                  (most-len (length most))
                  most-is-exact
                  (alternatives
                   (apply
                    'concat
                    (cdr (apply 'append
                                (mapcar '(lambda (com)
                                           (if (= (length com) most-len)
                                               ;; Most is one exact match,
                                               ;; note that and leave out
                                               ;; for later indication:
                                               (progn
                                                 (setq most-is-exact t)
                                                 ())
                                             (list ","
                                                   (substring com
                                                              most-len))))
                                        comps))))))
             (concat (and (> most-len (length name))
                          (concat open-bracket-determined
                                  (substring most (length name))
                                  close-bracket-determined))
                     open-bracket-prospects
                     (if most-is-exact
                         (concat "," alternatives)
                       alternatives)
                     close-bracket-prospects))))))

;;;_  - Initialization
(icomplete-prime-session)


;;;_ + Local emacs vars.
'(
Local variables:
eval: (save-excursion
        (if (not (condition-case err (outline-mode t)
                   (wrong-number-of-arguments nil)))
            (progn
              (message
               "Allout outline-mode not loaded, not adjusting buffer exposure")
              (sit-for 1))
          (message "Adjusting '%s' visibility" (buffer-name))
          (outline-lead-with-comment-string ";;;_")
          (goto-char 0)
          (outline-exposure -1 0)))
End:)

;;; icomplete.el ends here

