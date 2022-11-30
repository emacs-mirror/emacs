;;; org-goto.el --- Fast navigation in an Org buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2022 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org)
(require 'org-refile)

(defvar org-goto-exit-command nil)
(defvar org-goto-map nil)
(defvar org-goto-marker nil)
(defvar org-goto-selected-point nil)
(defvar org-goto-start-pos nil)
(defvar org-goto-window-configuration nil)

(defconst org-goto-local-auto-isearch-map (make-sparse-keymap))
(set-keymap-parent org-goto-local-auto-isearch-map isearch-mode-map)

(defconst org-goto-help
  "Browse buffer copy, to find location or copy text.%s
RET=jump to location             C-g=quit and return to previous location
\[Up]/[Down]=next/prev headline   TAB=cycle visibility   [/] org-occur")



;;; Customization

(defgroup org-goto nil
  "Options concerning Org Goto navigation interface."
  :tag "Org Goto"
  :group 'org)

(defcustom org-goto-interface 'outline
  "The default interface to be used for `org-goto'.

Allowed values are:

`outline'

   The interface shows an outline of the relevant file and the
   correct heading is found by moving through the outline or by
   searching with incremental search.

`outline-path-completion'

  Headlines in the current buffer are offered via completion.
  This is the interface also used by the refile command."
  :group 'org-goto
  :type '(choice
	  (const :tag "Outline" outline)
	  (const :tag "Outline-path-completion" outline-path-completion)))

(defcustom org-goto-max-level 5
  "Maximum target level when running `org-goto' with refile interface."
  :group 'org-goto
  :type 'integer)

(defcustom org-goto-auto-isearch t
  "Non-nil means typing characters in `org-goto' starts incremental search.
When nil, you can use these keybindings to navigate the buffer:

  q    Quit the Org Goto interface
  n    Go to the next visible heading
  p    Go to the previous visible heading
  f    Go one heading forward on same level
  b    Go one heading backward on same level
  u    Go one heading up"
  :group 'org-goto
  :type 'boolean)



;;; Internal functions

(defun org-goto--set-map ()
  "Set the keymap `org-goto'."
  (setq org-goto-map
	(let ((map (make-sparse-keymap)))
	  (let ((cmds '(isearch-forward isearch-backward kill-ring-save set-mark-command
					mouse-drag-region universal-argument org-occur)))
	    (dolist (cmd cmds)
	      (substitute-key-definition cmd cmd map global-map)))
	  (suppress-keymap map)
	  (org-defkey map "\C-m"     'org-goto-ret)
	  (org-defkey map [(return)] 'org-goto-ret)
	  (org-defkey map [(left)]   'org-goto-left)
	  (org-defkey map [(right)]  'org-goto-right)
	  (org-defkey map [(control ?g)] 'org-goto-quit)
	  (org-defkey map "\C-i" 'org-cycle)
	  (org-defkey map [(tab)] 'org-cycle)
	  (org-defkey map [(down)] 'outline-next-visible-heading)
	  (org-defkey map [(up)] 'outline-previous-visible-heading)
	  (if org-goto-auto-isearch
              (define-key-after map [t] 'org-goto-local-auto-isearch)
            (org-defkey map "q" 'org-goto-quit)
	    (org-defkey map "n" 'outline-next-visible-heading)
	    (org-defkey map "p" 'outline-previous-visible-heading)
	    (org-defkey map "f" 'outline-forward-same-level)
	    (org-defkey map "b" 'outline-backward-same-level)
	    (org-defkey map "u" 'outline-up-heading))
	  (org-defkey map "/" 'org-occur)
	  (org-defkey map "\C-c\C-n" 'outline-next-visible-heading)
	  (org-defkey map "\C-c\C-p" 'outline-previous-visible-heading)
	  (org-defkey map "\C-c\C-f" 'outline-forward-same-level)
	  (org-defkey map "\C-c\C-b" 'outline-backward-same-level)
	  (org-defkey map "\C-c\C-u" 'outline-up-heading)
	  map)))

;; `isearch-other-control-char' was removed in Emacs 24.4.
(if (fboundp 'isearch-other-control-char)
    (progn
      (define-key org-goto-local-auto-isearch-map "\C-i" 'isearch-other-control-char)
      (define-key org-goto-local-auto-isearch-map "\C-m" 'isearch-other-control-char))
  (define-key org-goto-local-auto-isearch-map "\C-i" nil)
  (define-key org-goto-local-auto-isearch-map "\C-m" nil)
  (define-key org-goto-local-auto-isearch-map [return] nil))

(defun org-goto--local-search-headings (string bound noerror)
  "Search and make sure that any matches are in headlines."
  (catch 'return
    (while (if isearch-forward
               (search-forward string bound noerror)
             (search-backward string bound noerror))
      (when (save-match-data
	      (and (save-excursion
		     (beginning-of-line)
		     (looking-at org-complex-heading-regexp))
		   (or (not (match-beginning 5))
		       (< (point) (match-beginning 5)))))
	(throw 'return (point))))))

(defun org-goto-local-auto-isearch ()
  "Start isearch."
  (interactive)
  (let ((keys (this-command-keys)))
    (when (eq (lookup-key isearch-mode-map keys) 'isearch-printing-char)
      (isearch-mode t)
      (isearch-process-search-char (string-to-char keys))
      (font-lock-ensure))))

(defun org-goto-ret (&optional _arg)
  "Finish `org-goto' by going to the new location."
  (interactive "P")
  (setq org-goto-selected-point (point))
  (setq org-goto-exit-command 'return)
  (throw 'exit nil))

(defun org-goto-left ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-at-heading-p)
      (progn
	(beginning-of-line 1)
	(setq org-goto-selected-point (point)
	      org-goto-exit-command 'left)
	(throw 'exit nil))
    (user-error "Not on a heading")))

(defun org-goto-right ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-at-heading-p)
      (progn
	(setq org-goto-selected-point (point)
	      org-goto-exit-command 'right)
	(throw 'exit nil))
    (user-error "Not on a heading")))

(defun org-goto-quit ()
  "Finish `org-goto' without cursor motion."
  (interactive)
  (setq org-goto-selected-point nil)
  (setq org-goto-exit-command 'quit)
  (throw 'exit nil))



;;; Public API

;;;###autoload
(defun org-goto-location (&optional _buf help)
  "Let the user select a location in current buffer.
This function uses a recursive edit.  It returns the selected
position or nil."
  (let ((isearch-mode-map org-goto-local-auto-isearch-map)
	(isearch-hide-immediately nil)
	(isearch-search-fun-function
	 (lambda () #'org-goto--local-search-headings))
	(help (or help org-goto-help)))
    (save-excursion
      (save-window-excursion
	(delete-other-windows)
	(and (get-buffer "*org-goto*") (kill-buffer "*org-goto*"))
	(pop-to-buffer-same-window
	 (condition-case nil
	     (make-indirect-buffer (current-buffer) "*org-goto*" t)
	   (error (make-indirect-buffer (current-buffer) "*org-goto*" t))))
	(let (temp-buffer-show-function temp-buffer-show-hook)
	  (with-output-to-temp-buffer "*Org Help*"
	    (princ (format help (if org-goto-auto-isearch
				    "  Just type for auto-isearch."
				  "  n/p/f/b/u to navigate, q to quit.")))))
	(org-fit-window-to-buffer (get-buffer-window "*Org Help*"))
	(org-cycle-overview)
	(setq buffer-read-only t)
	(if (and (boundp 'org-goto-start-pos)
		 (integer-or-marker-p org-goto-start-pos))
	    (progn (goto-char org-goto-start-pos)
		   (when (org-invisible-p)
		     (org-fold-show-set-visibility 'lineage)))
	  (goto-char (point-min)))
	(let (org-special-ctrl-a/e) (org-beginning-of-line))
	(message "Select location and press RET")
	(use-local-map org-goto-map)
	(recursive-edit)))
    (kill-buffer "*org-goto*")
    (cons org-goto-selected-point org-goto-exit-command)))

;;;###autoload
(defun org-goto (&optional alternative-interface)
  "Look up a different location in the current file, keeping current visibility.

When you want look-up or go to a different location in a
document, the fastest way is often to fold the entire buffer and
then dive into the tree.  This method has the disadvantage, that
the previous location will be folded, which may not be what you
want.

This command works around this by showing a copy of the current
buffer in an indirect buffer, in overview mode.  You can dive
into the tree in that copy, use `org-occur' and incremental search
to find a location.  When pressing RET or `Q', the command
returns to the original buffer in which the visibility is still
unchanged.  After RET it will also jump to the location selected
in the indirect buffer and expose the headline hierarchy above.

With a prefix argument, use the alternative interface: e.g., if
`org-goto-interface' is `outline' use `outline-path-completion'."
  (interactive "P")
  (org-goto--set-map)
  (let* ((org-refile-targets `((nil . (:maxlevel . ,org-goto-max-level))))
	 (org-refile-use-outline-path t)
	 (org-refile-target-verify-function nil)
	 (interface
	  (if (not alternative-interface)
	      org-goto-interface
	    (if (eq org-goto-interface 'outline)
		'outline-path-completion
	      'outline)))
	 (org-goto-start-pos (point))
	 (selected-point
	  (if (eq interface 'outline) (car (org-goto-location))
	    (let ((pa (org-refile-get-location "Goto")))
	      (org-refile-check-position pa)
	      (nth 3 pa)))))
    (if selected-point
	(progn
	  (org-mark-ring-push org-goto-start-pos)
	  (goto-char selected-point)
	  (when (or (org-invisible-p) (org-invisible-p2))
	    (org-fold-show-context 'org-goto)))
      (message "Quit"))))

(provide 'org-goto)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-goto.el ends here
