;;; mh-init.el --- MH-E initialization.

;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.

;; Author: Peter S. Galbraith <psg@debian.org>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;; Sets up the MH variant (currently nmh or MH).
;;
;; Users may customize `mh-variant' to switch between available variants.
;; Available MH variants are described in the variable `mh-variants'.
;; Developers may check which variant is currently in use with the
;; variable `mh-variant-in-use' or the function `mh-variant-p'.

;;; Change Log:

;;; Code:

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'mh-utils)

;;; Avoid compiler warnings.
(eval-when-compile (defvar image-load-path))

;;; Set for local environment:
;;; mh-progs and mh-lib used to be set in paths.el, which tried to
;;; figure out at build time which of several possible directories MH
;;; was installed into.  But if you installed MH after building Emacs,
;;; this would almost certainly be wrong, so now we do it at run time.

(defvar mh-progs nil
  "Directory containing MH commands, such as inc, repl, and rmm.")

(defvar mh-lib nil
  "Directory containing the MH library.
This directory contains, among other things, the components file.")

(defvar mh-lib-progs nil
  "Directory containing MH helper programs.
This directory contains, among other things, the mhl program.")

(defvar mh-flists-present-flag nil
  "Non-nil means that we have `flists'.")

;;;###autoload
(put 'mh-progs 'risky-local-variable t)
;;;###autoload
(put 'mh-lib 'risky-local-variable t)
;;;###autoload
(put 'mh-lib-progs 'risky-local-variable t)

(defvar mh-variant-in-use nil
  "The MH variant currently in use; a string with variant and version number.
This differs from `mh-variant' when the latter is set to `autodetect'.")

;;;###mh-autoload
(defun mh-variant-set (variant)
  "Set the MH variant to VARIANT.
Sets `mh-progs', `mh-lib', `mh-lib-progs' and `mh-flists-present-flag'.
If the VARIANT is `autodetect', then first try nmh, then MH and finally
GNU mailutils."
  (interactive
   (list (completing-read
          "MH Variant: "
          (mapcar (lambda (x) (list (car x))) (mh-variants))
          nil t)))
  (let ((valid-list (mapcar (lambda (x) (car x)) (mh-variants))))
    (cond
     ((eq variant 'none))
     ((eq variant 'autodetect)
      (cond
       ((mh-variant-set-variant 'nmh)
        (message "%s installed as MH variant" mh-variant-in-use))
       ((mh-variant-set-variant 'mh)
        (message "%s installed as MH variant" mh-variant-in-use))
       ((mh-variant-set-variant 'mu-mh)
        (message "%s installed as MH variant" mh-variant-in-use))
       (t
        (message "No MH variant found on the system!"))))
     ((member variant valid-list)
      (when (not (mh-variant-set-variant variant))
        (message "Warning: %s variant not found.  Autodetecting..." variant)
        (mh-variant-set 'autodetect)))
     (t
      (message "Unknown variant.  Use %s"
               (mapconcat '(lambda (x) (format "%s" (car x)))
                          mh-variants " or "))))))

(defun mh-variant-set-variant (variant)
  "Setup the system variables for the MH variant named VARIANT.
If VARIANT is a string, use that key in the variable `mh-variants'.
If VARIANT is a symbol, select the first entry that matches that variant."
  (cond
   ((stringp variant)                   ;e.g. "nmh 1.1-RC1"
    (when (assoc variant mh-variants)
      (let* ((alist (cdr (assoc variant mh-variants)))
             (lib-progs (cadr (assoc 'mh-lib-progs alist)))
             (lib       (cadr (assoc 'mh-lib       alist)))
             (progs     (cadr (assoc 'mh-progs     alist)))
             (flists    (cadr (assoc 'flists       alist))))
        ;;(set-default mh-variant variant)
        (setq mh-x-mailer-string     nil
              mh-flists-present-flag flists
              mh-lib-progs           lib-progs
              mh-lib                 lib
              mh-progs               progs
              mh-variant-in-use      variant))))
   ((symbolp variant)                   ;e.g. 'nmh (pick the first match)
    (loop for variant-list in mh-variants
          when (eq variant (cadr (assoc 'variant (cdr variant-list))))
          return (let* ((version   (car variant-list))
                        (alist (cdr variant-list))
                        (lib-progs (cadr (assoc 'mh-lib-progs alist)))
                        (lib       (cadr (assoc 'mh-lib       alist)))
                        (progs     (cadr (assoc 'mh-progs     alist)))
                        (flists    (cadr (assoc 'flists       alist))))
                   ;;(set-default mh-variant flavor)
                   (setq mh-x-mailer-string     nil
                         mh-flists-present-flag flists
                         mh-lib-progs           lib-progs
                         mh-lib                 lib
                         mh-progs               progs
                         mh-variant-in-use      version)
                   t)))))

;;;###mh-autoload
(defun mh-variant-p (&rest variants)
  "Return t if variant is any of VARIANTS.
Currently known variants are 'MH, 'nmh, and 'mu-mh."
  (let ((variant-in-use
         (cadr (assoc 'variant (assoc mh-variant-in-use mh-variants)))))
    (not (null (member variant-in-use variants)))))

(defvar mh-sys-path
  '("/usr/local/nmh/bin"                ; nmh default
    "/usr/local/bin/mh/"
    "/usr/local/mh/"
    "/usr/bin/mh/"                      ; Ultrix 4.2, Linux
    "/usr/new/mh/"                      ; Ultrix < 4.2
    "/usr/contrib/mh/bin/"              ; BSDI
    "/usr/pkg/bin/"                     ; NetBSD
    "/usr/local/bin/"
    "/usr/local/bin/mu-mh/"             ; GNU mailutils - default
    "/usr/bin/mu-mh/")                  ; GNU mailutils - packaged
  "List of directories to search for variants of the MH variant.
The list `exec-path' is searched in addition to this list.
There's no need for users to modify this list.  Instead add extra
directories to the customizable variable `mh-path'.")

(defcustom mh-path nil
  "*List of directories to search for variants of the MH variant.
The directories will be searched for `mhparam' in addition to directories
listed in `mh-sys-path' and `exec-path'."
  :group 'mh-e
  :type '(repeat (directory)))

(defvar mh-variants nil
  "List describing known MH variants.
Created by the function `mh-variants'")

(defun mh-variant-mh-info (dir)
  "Return info for MH variant in DIR assuming a temporary buffer is setup."
  ;; MH does not have the -version option.
  ;; Its version number is included in the output of `-help' as:
  ;;
  ;; version: MH 6.8.4 #2[UCI] (burrito) of Fri Jan 15 20:01:39 EST 1999
  ;; options: [ATHENA] [BIND] [DUMB] [LIBLOCKFILE] [LOCALE] [MAILGROUP] [MHE]
  ;;          [MHRC] [MIME] [MORE='"/usr/bin/sensible-pager"'] [NLINK_HACK]
  ;;          [NORUSERPASS] [OVERHEAD] [POP] [POPSERVICE='"pop-3"'] [RENAME]
  ;;          [RFC1342] [RPATHS] [RPOP] [SENDMTS] [SMTP] [SOCKETS]
  ;;          [SPRINTFTYPE=int] [SVR4] [SYS5] [SYS5DIR] [TERMINFO]
  ;;          [TYPESIG=void] [UNISTD] [UTK] [VSPRINTF]
  (let ((mhparam (expand-file-name "mhparam" dir)))
    (when (and (file-exists-p mhparam) (file-executable-p mhparam))
      (erase-buffer)
      (call-process mhparam nil '(t nil) nil "-help")
      (goto-char (point-min))
      (when (search-forward-regexp "version: MH \\(\\S +\\)" nil t)
        (let ((version (format "MH %s" (match-string 1))))
          (erase-buffer)
          (call-process mhparam nil '(t nil) nil "libdir")
          (goto-char (point-min))
          (when (search-forward-regexp "^.*$" nil t)
            (let ((libdir (match-string 0)))
              `(,version
                (variant        mh)
                (mh-lib-progs   ,libdir)
                (mh-lib         ,libdir)
                (mh-progs       ,dir)
                (flists         nil)))))))))

(defun mh-variant-mu-mh-info (dir)
  "Return info for GNU mailutils variant in DIR.
This assumes that a temporary buffer is setup."
  ;; 'mhparam -version' output:
  ;; mhparam (GNU mailutils 0.3.2)
  (let ((mhparam (expand-file-name "mhparam" dir)))
    (when (and (file-exists-p mhparam) (file-executable-p mhparam))
      (erase-buffer)
      (call-process mhparam nil '(t nil) nil "-version")
      (goto-char (point-min))
      (when (search-forward-regexp "mhparam (\\(GNU [Mm]ailutils \\S +\\))"
                                   nil t)
        (let ((version (match-string 1)))
          (erase-buffer)
          (call-process mhparam nil '(t nil) nil "libdir" "etcdir")
          (goto-char (point-min))
          (when (search-forward-regexp "^libdir:\\s-\\(\\S-+\\)\\s-*$" nil t)
            (let ((libdir (match-string 1)))
              (goto-char (point-min))
              (when (search-forward-regexp
                     "^etcdir:\\s-\\(\\S-+\\)\\s-*$" nil t)
                (let ((etcdir (match-string 1))
                      (flists (file-exists-p (expand-file-name "flists" dir))))
                  `(,version
                    (variant        mu-mh)
                    (mh-lib-progs   ,libdir)
                    (mh-lib         ,etcdir)
                    (mh-progs       ,dir)
                    (flists         ,flists)))))))))))

(defun mh-variant-nmh-info (dir)
  "Return info for nmh variant in DIR assuming a temporary buffer is setup."
  ;; `mhparam -version' outputs:
  ;; mhparam -- nmh-1.1-RC1 [compiled on chaak at Fri Jun 20 11:03:28 PDT 2003]
  (let ((mhparam (expand-file-name "mhparam" dir)))
    (when (and (file-exists-p mhparam) (file-executable-p mhparam))
      (erase-buffer)
      (call-process mhparam nil '(t nil) nil "-version")
      (goto-char (point-min))
      (when (search-forward-regexp "mhparam -- nmh-\\(\\S +\\)" nil t)
        (let ((version (format "nmh %s" (match-string 1))))
          (erase-buffer)
          (call-process mhparam nil '(t nil) nil "libdir" "etcdir")
          (goto-char (point-min))
          (when (search-forward-regexp "^libdir:\\s-\\(\\S-+\\)\\s-*$" nil t)
            (let ((libdir (match-string 1)))
              (goto-char (point-min))
              (when (search-forward-regexp
                     "^etcdir:\\s-\\(\\S-+\\)\\s-*$" nil t)
                (let ((etcdir (match-string 1))
                      (flists (file-exists-p (expand-file-name "flists" dir))))
                  `(,version
                    (variant        nmh)
                    (mh-lib-progs   ,libdir)
                    (mh-lib         ,etcdir)
                    (mh-progs       ,dir)
                    (flists         ,flists)))))))))))

(defun mh-variant-info (dir)
  "Return MH variant found in DIR, or nil if none present."
  (save-excursion
    (let ((tmp-buffer (get-buffer-create mh-temp-buffer)))
      (set-buffer tmp-buffer)
      (cond
       ((mh-variant-mh-info dir))
       ((mh-variant-nmh-info dir))
       ((mh-variant-mu-mh-info dir))))))

;;;###mh-autoload
(defun mh-variants ()
  "Return a list of installed variants of MH on the system.
This function looks for MH in `mh-sys-path', `mh-path' and
`exec-path'. The format of the list of variants that is returned is described
by the variable `mh-variants'."
  (if mh-variants
      mh-variants
    (let ((list-unique))
      ;; Make a unique list of directories, keeping the given order.
      ;; We don't want the same MH variant to be listed multiple times.
      (loop for dir in (append mh-path mh-sys-path exec-path) do
            (setq dir (file-chase-links (directory-file-name dir)))
            (add-to-list 'list-unique dir))
      (loop for dir in (nreverse list-unique) do
            (when (and dir (file-directory-p dir) (file-readable-p dir))
              (let ((variant (mh-variant-info dir)))
                (if variant
                    (add-to-list 'mh-variants variant)))))
      mh-variants)))

;;; XXX The two calls to message in this function should really be calls to
;;; error. However, when this function is compiled via the top-level call in
;;; mh-customize.el, it is actually called, and in a compile environment, the
;;; errors are triggered which botches the compile. As a workaround, the calls
;;; to error have been changed to calls to message, and code following was
;;; inserted as an else clause. This is not robust, so if you can fix this,
;;; please do!

(defvar mh-image-load-path-called-flag nil)

;;;###mh-autoload
(defun mh-image-load-path ()
  "Ensure that the MH-E images are accessible by `find-image'.
Images for MH-E are found in ../../etc/images relative to the files in
`lisp/mh-e'. If `image-load-path' exists (since Emacs 22), then the images
directory is added to it if isn't already there. Otherwise, the images
directory is added to the `load-path' if it isn't already there."
  (message "mh-image-load-path called") ;XXX: for debugging
  (unless mh-image-load-path-called-flag
    (let (mh-load-path mh-image-load-path)
      ;; First, find mh-e in the load-path.
      (setq mh-load-path
            (loop for dir in load-path
                  for dir-name = (directory-file-name dir)
                  when (and (equal (file-name-nondirectory dir-name) "mh-e")
                            (file-exists-p dir-name))
                  return dir-name))
      (if mh-load-path
          (setq mh-image-load-path
                (expand-file-name (concat (file-name-directory mh-load-path)
                                          "../etc/images")))
        (error "Can not find mh-e in load-path"))
      (cond ((or (not mh-image-load-path)
                 (not (file-exists-p mh-image-load-path)))
             (error "Can not find image directory %s"
                    mh-image-load-path))
            ((boundp 'image-load-path)
             (pushnew mh-image-load-path image-load-path))
            ((not (member mh-image-load-path load-path))
             (push mh-image-load-path load-path))))
    (setq mh-image-load-path-called-flag t)))

(provide 'mh-init)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;; arch-tag: e8372aeb-d803-42b1-9c95-3c93ad22f63c
;;; mh-init.el ends here
