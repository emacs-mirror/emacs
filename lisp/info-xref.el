;;; info-xref.el --- check external references in an Info document

;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.

;; Author: Kevin Ryde <user42@zip.com.au>
;; Keywords: docs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file implements some simple checking of external cross references in
;; info files, by attempting to visit the nodes specified.
;;
;; "makeinfo" checks references internal to a document, but not external
;; references, which makes it rather easy for mistakes to creep in or node
;; name changes to go unnoticed.  `Info-validate' doesn't check external
;; references either.
;;
;; `M-x info-xref-check' checks one file.  When invoked from an Info-mode or
;; texinfo-mode buffer, the current info file is the default at the prompt.
;;
;; `M-x info-xref-check-all' looks at everything in the normal info path.
;; This might be a lot of files but it's a good way to check the consistency
;; of the whole system.
;;
;; Results are shown in a buffer.  The format is a bit rough, but hopefully
;; there won't be too many problems normally, and correcting them is a
;; manual process anyway, a case of finding the right spot in the original
;; .texi and finding what node it ought to point to.
;;
;; When a target info file doesn't exist there's clearly no way to validate
;; node references within it.  A message is given for missing target files
;; (once per source document), it could be simply that the target hasn't
;; been installed, or it could be a mistake in the reference.
;;
;; Indirect info files are understood, just pass the top-level foo.info to
;; `info-xref-check' and it traverses all sub-files.  Compressed info files
;; are accepted too, as usual for `Info-mode'.
;;
;; `info-xref-check-all' is rather permissive in what it considers an info
;; file.  It has to be since info files don't necessarily have a ".info"
;; suffix (eg. this is usual for the emacs manuals).  One consequence of
;; this is that if for instance there's a source code directory in
;; `Info-directory-list' then a lot of extraneous files might be read, which
;; will be time consuming but should be harmless.
;;
;; `M-x info-xref-check-all-custom' is a related command, it goes through
;; all info document references in customizable variables, checking them
;; like info file cross references.

;;; Code:

(require 'info)

(defconst info-xref-results-buffer "*info-xref results*"
  "Name of the buffer for info-xref results.")

;;;###autoload
(defun info-xref-check (filename)
  "Check external references in FILENAME, an info document."
  (interactive
   (list
    (let* ((default-filename
             (cond ((eq major-mode 'Info-mode)
                    Info-current-file)
                   ((eq major-mode 'texinfo-mode)
                    ;; look for @setfilename like makeinfo.el does
                    (save-excursion
                      (goto-char (point-min))
                      (if (re-search-forward
                           "^@setfilename[ \t]+\\([^ \t\n]+\\)[ \t]*"
                           (line-beginning-position 100) t)
                          (expand-file-name (match-string 1)))))))
           (prompt (if default-filename
                       (format "Info file (%s): " default-filename)
                     "Info file: ")))
      (read-file-name prompt nil default-filename t))))
  (info-xref-check-list (list filename)))

;;;###autoload
(defun info-xref-check-all ()
  "Check external references in all info documents in the usual path.
The usual path is `Info-directory-list' and `Info-additional-directory-list'."
  (interactive)
  (info-xref-check-list (info-xref-all-info-files)))

;; An alternative to trying to get only top-level files here would be to
;; simply return all files, and have info-xref-check-list not follow
;; Indirect:.  The current way seems a bit nicer though, because it gets the
;; proper top-level filename into the error messages, and suppresses
;; duplicate "not available" messages for all subfiles of a single document.

(defun info-xref-all-info-files ()
  "Return a list of all available info files.
Only top-level files are returned, subfiles are excluded.

Since info files don't have to have a .info suffix, all files in the
relevant directories are considered, which might mean a lot of extraneous
things are returned if for instance a source code directory is in the path."

  (info-initialize) ;; establish Info-directory-list
  (apply 'nconc
         (mapcar
          (lambda (dir)
            (let ((result nil))
              (dolist (name (directory-files dir t))
                (unless (or (file-directory-p name) (info-xref-subfile-p name))
		  (push name result)))
              (nreverse result)))
          (append Info-directory-list Info-additional-directory-list))))

(defun info-xref-subfile-p (filename)
  "Return t if FILENAME is an info subfile.
If removing the last \"-<NUM>\" from the filename gives a file that exists,
then consider FILENAME a subfile.  This is an imperfect test, we probably
should open up the purported top file and see what subfiles it says."
  (and (string-match "\\`\\(\\([^-]*-\\)*[^-]*\\)-[0-9]+\\(.*\\)\\'" filename)
       (file-exists-p (concat (match-string 1 filename)
                              (match-string 3 filename)))))


;; Some dynamic variables are used to share information with sub-functions
;; below.
;;
;; info-xref-filename - current top-level filename, eg. /usr/info/foo.info.gz
;;
;; info-xref-filename-header - a heading message for the current top-level
;;     filename, or "" when it's been printed.
;;
;; info-xref-good - count of good cross references.
;;
;; info-xref-bad - count of bad cross references.
;;
;; info-xref-xfile-alist - indexed by "(foo)" with value nil or t according
;;     to whether "(foo)" exists or not.  This is used to suppress duplicate
;;     messages about foo not being available.  (Duplicates within one
;;     top-level file that is.)

(defun info-xref-check-list (filename-list)
  "Check external references in info documents in FILENAME-LIST."
  (pop-to-buffer info-xref-results-buffer t)
  (erase-buffer)
  (let ((info-xref-good 0)
        (info-xref-bad  0))
    (dolist (info-xref-filename filename-list)
      (let ((info-xref-filename-heading
             (format "In file %s:\n" info-xref-filename))
            (info-xref-xfile-alist nil))
        (with-temp-message (format "Looking at %s" info-xref-filename)
          (with-temp-buffer
            (info-insert-file-contents info-xref-filename)
            (goto-char (point-min))
            (if (re-search-forward "\^_\nIndirect:\n" nil t)
                (let ((dir (file-name-directory info-xref-filename)))
                  (while (looking-at "\\(.*\\): [0-9]+\n")
                    (let ((subfile (match-string 1)))
                      (with-temp-buffer
                        (info-insert-file-contents
                         (expand-file-name subfile dir))
                        (info-xref-check-buffer)))
                    (forward-line)))
              (info-xref-check-buffer))))))
    (insert (format "done, %d good, %d bad\n" info-xref-good info-xref-bad))))

(defun info-xref-check-buffer ()
  "Check external references in the info file in the current buffer.
This should be the raw file contents, not `Info-mode'."
  (goto-char (point-min))
  (while (re-search-forward
          "\\*[Nn]ote[ \n\t]+[^:]*:[ \n\t]+\\(\\(([^)]*)\\)[^.,]+\\)[.,]"
          nil t)
    (let* ((file (match-string 2))
           (node ;; Canonicalize spaces: we could use "[\t\n ]+" but
	    ;; we try to avoid uselessly replacing " " with " ".
	    (replace-regexp-in-string "[\t\n][\t\n ]*\\| [\t\n ]+" " "
				      (match-string 1) t t)))
      (if (string-equal "()" file)
          (info-xref-output "Empty filename part: %s\n" node)
        ;; see if the file exists, if we haven't tried it before
        (unless (assoc file info-xref-xfile-alist)
          (let ((found (info-xref-goto-node-p file)))
            (push (cons file found) info-xref-xfile-alist)
            (unless found
              (info-xref-output "Not available to check: %s\n" file))))
        ;; if the file exists, try the node
        (when (cdr (assoc file info-xref-xfile-alist))
          (if (info-xref-goto-node-p node)
              (setq info-xref-good (1+ info-xref-good))
            (setq info-xref-bad (1+ info-xref-bad))
            (info-xref-output "No such node: %s\n" node)))))))

(defun info-xref-output (str &rest args)
  "Emit a `format'-ed message STR+ARGS to the info-xref output buffer."
  (with-current-buffer info-xref-results-buffer
    (insert info-xref-filename-heading
            (apply 'format str args))
    (setq info-xref-filename-heading "")
    ;; all this info-xref can be pretty slow, display now so the user can
    ;; see some progress
    (sit-for 0)))

;; When asking Info-goto-node to fork, *info* needs to be the current
;; buffer, otherwise it seems to clone the current buffer but then do the
;; goto-node in plain *info*.
;;
;; We only fork if *info* already exists, if it doesn't then we can create
;; and destroy just that instead of a new name.
;;
;; If Info-goto-node can't find the file, then no new buffer is created.  If
;; it finds the file but not the node, then a buffer is created.  Handle
;; this difference by checking before killing.
;;
(defun info-xref-goto-node-p (node)
  "Return t if it's possible to go to the given NODE."
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (save-window-excursion
        (prog1
            (condition-case err
                (progn
                  (Info-goto-node node
                                  (when (get-buffer "*info*")
                                    (set-buffer "*info*")
                                    "xref - temporary"))
                  t)
              (error nil))
          (unless (equal (current-buffer) oldbuf)
            (kill-buffer (current-buffer))))))))

;;;###autoload
(defun info-xref-check-all-custom ()
  "Check info references in all customize groups and variables.
`custom-manual' and `info-link' entries in the `custom-links' list are checked.

`custom-load' autoloads for all symbols are loaded in order to get all the
link information.  This will be a lot of lisp packages loaded, and can take
quite a while."

  (interactive)
  (pop-to-buffer info-xref-results-buffer t)
  (erase-buffer)
  (let ((info-xref-filename-heading ""))

    ;; `custom-load-symbol' is not used, since it quietly ignores errors,
    ;; but we want to show them (since they may mean incomplete checking).
    ;;
    ;; Just one pass through mapatoms is made.  There shouldn't be any new
    ;; custom-loads setup by packages loaded.
    ;;
    (info-xref-output "Loading custom-load autoloads ...\n")
    (require 'cus-start)
    (require 'cus-load)
    (let ((viper-mode nil)) ;; tell viper.el not to ask about viperizing
      (mapatoms
       (lambda (symbol)
         (dolist (load (get symbol 'custom-loads))
           (cond ((symbolp load)
                  (condition-case cause (require load)
                    (error
                     (info-xref-output "Symbol `%s': cannot require '%s: %s\n"
                                       symbol load cause))))
                 ;; skip if previously loaded
                 ((assoc load load-history))
                 ((assoc (locate-library load) load-history))
                 (t
                  (condition-case cause (load load)
                    (error
                     (info-xref-output "Symbol `%s': cannot load \"%s\": %s\n"
                                       symbol load cause)))))))))

    ;; Don't bother to check whether the info file exists as opposed to just
    ;; a missing node.  If you have the lisp then you should have the
    ;; documentation, so missing node name will be the usual fault.
    ;;
    (info-xref-output "\nChecking custom-links references ...\n")
    (let ((good 0)
          (bad  0))
      (mapatoms
       (lambda (symbol)
         (dolist (link (get symbol 'custom-links))
           (when (memq (car link) '(custom-manual info-link))
             (if (info-xref-goto-node-p (cadr link))
                 (setq good (1+ good))
               (setq bad (1+ bad))
               ;; symbol-file gives nil for preloaded variables, would need
               ;; to copy what describe-variable does to show the right place
               (info-xref-output "Symbol `%s' (in %s): cannot goto node: %s\n"
                                 symbol (symbol-file symbol) (cadr link)))))))
      (info-xref-output "%d good, %d bad\n" good bad))))

(provide 'info-xref)

;;; arch-tag: 69d4d528-69ed-4cc2-8eb4-c666a0c1d5ac
;;; info-xref.el ends here
