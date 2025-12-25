;;; package-autosuggest.el --- Automatic suggestion of relevant packages -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Packages are suggested to users depending on the entries in the
;; "auto-suggestion" database that is bundled with Emacs.  By enabling
;; the `package-autosuggest-mode' minor mode, Emacs will hint that files
;; without any special support, but the user can manually query
;; suggestions using the `package-autosuggest' command as well.

;;; Code:

(require 'package-core)
(require 'package-install)

(defgroup package-autosuggest nil
  "Automatic suggestion of relevant packages."
  :group 'package
  :version "31.1")

(defconst package-autosuggest-database
  (eval-when-compile
    (if-let* ((db (expand-file-name "package-autosuggest.eld" data-directory))
              ((file-exists-p db)))
      (with-temp-buffer
        (insert-file-contents db)
        (read (current-buffer)))))
  "List of hints for packages to suggest installing.
Each hint has the form (PACKAGE TYPE DATA), where PACKAGE is a symbol
denoting the package and major-mode the hint applies to, TYPE is one of
`auto-mode-alist', `magic-mode-alist' or `interpreter-mode-alist'
indicating the type of check to be made and DATA is the value to check
against TYPE in the intuitive way (e.g. for `auto-mode-alist' DATA is a
regular expression matching a file name that PACKAGE should be suggested
for).  If the package name and the major mode name differ, then an
optional forth element MAJOR-MODE can indicate what command to invoke to
enable the package.")

(defcustom package-autosuggest-style 'mode-line
  "How to draw attention to `package-autosuggest-mode' suggestions.
You can set this value to `mode-line' (default) to indicate the
availability of a package suggestion in the minor mode, `always' to
prompt the user in the minibuffer every time a suggestion is available
in a `fundamenta-mode' buffer, `once' to do only prompt the user once
for each suggestion or `message' to just display a message hinting at
the existence of a suggestion."
  :type '(choice (const :tag "Indicate in mode line" mode-line)
                 (const :tag "Always prompt" always)
                 (const :tag "Prompt only once" once)
                 (const :tag "Indicate with message" message)))

;;;###autoload
(define-minor-mode package-autosuggest-mode
  "Enable the automatic suggestion and installation of packages."
  :global t
  (funcall (if package-autosuggest-mode #'add-hook #'remove-hook)
           'after-change-major-mode-hook
           #'package--autosuggest-after-change-mode))

(defvar package--autosuggest-suggested '()
  "List of packages that have already been suggested.
The elements of this list should be a subset of elements from
`package-autosuggest-database'.  Suggestions found in this list will not
count as suggestions (e.g. if `package-autosuggest-style' is set to
`mode-line', a suggestion found in here will inhibit
`package-autosuggest-mode' from displaying a hint in the mode line).")

(defun package--suggestion-applies-p (sug)
  "Check if a suggestion SUG is applicable to the current buffer.
SUG should be an element of `package-autosuggest-database'."
  (pcase sug
    (`(,(or (pred (lambda (e) (assq e package--autosuggest-suggested)))
            (pred package-installed-p))
       . ,_)
     nil)
    ((or `(,_ auto-mode-alist ,ext ,_)
         `(,_ auto-mode-alist ,ext))
     (and (string-match-p ext (buffer-name)) t))
    ((or `(,_ magic-mode-alist ,mag ,_)
         `(,_ magic-mode-alist ,mag))
     (save-restriction
       (widen)
       (save-excursion
         (goto-char (point-min))
         (looking-at-p mag))))
    ((or `(,_ interpreter-mode-alist ,magic ,_)
         `(,_ interpreter-mode-alist ,magic))
     (save-restriction
       (widen)
       (save-excursion
         (goto-char (point-min))
         (and (looking-at auto-mode-interpreter-regexp)
              (string-match-p
               (concat "\\`" (file-name-nondirectory (match-string 2)) "\\'")
               magic)))))))

(defun package--autosuggest-find-candidates ()
  "Return a list of suggestions that might be interesting the current buffer.
The elements of the returned list will be a subset of the elements of
`package--autosuggest-suggested'."
  (and package-autosuggest-mode (eq major-mode 'fundamental-mode)
       (let (suggetions)
         (dolist (sug package-autosuggest-database)
           (when (package--suggestion-applies-p sug)
             (push sug suggetions)))
         suggetions)))

(defun package--autosuggest-install-and-enable (sug)
  "Install and enable a package suggestion PKG-ENT.
SUG should be an element of `package-autosuggest-database'."
  (let ((buffers-to-update '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eq major-mode 'fundamental-mode) (buffer-file-name)
                   (package--suggestion-applies-p sug))
          (push buf buffers-to-update))))
    (with-demoted-errors "Failed to install package: %S"
      (package-install (car sug))
      (dolist (buf buffers-to-update)
        (with-demoted-errors "Failed to enable major mode: %S"
          (with-current-buffer buf
            (funcall-interactively (or (cadddr sug) (car sug)))))))))

(defvar package--autosugest-line-format
  '(:eval (package--autosugest-line-format)))
(put 'package--autosugest-line-format 'risky-local-variable t)

(defface package-autosuggest-face
  '((t :inherit (success)))
  "Face to use in the mode line to highlight suggested packages."
  :version "30.1")

(defun package--autosugest-line-format ()
  "Generate a mode-line string to indicate a suggested package."
  `(,@(and-let* (((not (null package-autosuggest-mode)))
                 ((eq package-autosuggest-style 'mode-line))
                 (avail (package--autosuggest-find-candidates)))
        (propertize
         (format "Install %s?"
                 (mapconcat
                  #'symbol-name
                  (delete-dups (mapcar #'car avail))
                  ", "))
         'face 'package-autosuggest-face
         'mouse-face 'mode-line-highlight
         'help-echo "Click to install suggested package."
         'keymap (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line down-mouse-1] #'package-autosuggest)
                   map)))))

;;;###autoload
(progn
  (add-to-list
   'mode-line-misc-info
   '(package-autosuggest-mode ("" package--autosugest-line-format))))

(defun package--autosuggest-after-change-mode ()
  "Display package suggestions for the current buffer.
This function should be added to `after-change-major-mode-hook'."
  (when-let* ((avail (package--autosuggest-find-candidates))
              (pkgs (mapconcat #'symbol-name
                               (delete-dups (mapcar #'car avail))
                               ", ")))
    (pcase-exhaustive package-autosuggest-style
      ('mode-line
       (force-mode-line-update t))
      ('always
       (when (yes-or-no-p (format "Install suggested packages (%s)?" pkgs))
         (mapc #'package--autosuggest-install-and-enable avail)))
      ('once
       (when (yes-or-no-p (format "Install suggested packages (%s)?" pkgs))
         (mapc #'package--autosuggest-install-and-enable avail))
       (setq package--autosuggest-suggested (append avail package--autosuggest-suggested)))
      ('message
       (message
        (substitute-command-keys
         (format "Found suggested packages: %s.  Install using  \\[package-autosuggest]"
                 pkgs)))))))

;;;###autoload
(defun package-autosuggest ()
  "Prompt the user to install the suggested packages."
  (interactive)
  (let* ((avail (or (package--autosuggest-find-candidates)
                    (user-error "No suggestions found")))
         (use-dialog-box t)
         (prompt (concat
                  "Install "
                  (mapconcat
                   #'symbol-name
                   (delete-dups (mapcar #'car avail))
                   ", ")
                  "?")))
    (if (yes-or-no-p prompt)
        (mapc #'package--autosuggest-install-and-enable avail)
      (setq package--autosuggest-suggested (append avail package--autosuggest-suggested))
      (when (eq package-autosuggest-style 'mode-line)
        (force-mode-line-update t)))))

(defun package-reset-suggestions ()
  "Forget previous package suggestions.
Emacs will remember if you have previously rejected a suggestion during
a session and won't mention it afterwards.  If you have made a mistake
or would like to reconsider this, use this command to want to reset the
suggestions."
  (interactive)
  (setq package--autosuggest-suggested nil))

(provide 'package-autosuggest)
;;; package-autosuggest.el ends here
