;;; mh-identity.el --- Multiple identify support for MH-E.

;; Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Multiple identity support for MH-E.
;;
;; Used to easily set different fields such as From and Organization, as
;; well as different signature files.
;;
;; Customize the variable `mh-identity-list' and an Identity menu will
;; appear in mh-letter-mode.  The command 'mh-insert-identity can be used
;; from the command line.

;;; Change Log:

;;; Code:


(require 'cl)

(eval-when (compile load eval)
  (defvar mh-comp-loaded nil)
  (unless mh-comp-loaded
    (setq mh-comp-loaded t)
    (require 'mh-comp)))                   ;Since we do this on sending

(autoload 'mml-insert-tag "mml")

;;;###mh-autoload
(defun mh-identity-make-menu ()
  "Build (or rebuild) the Identity menu (e.g. after the list is modified)."
  (when (and mh-identity-list (boundp 'mh-letter-mode-map))
    (easy-menu-define mh-identity-menu mh-letter-mode-map
      "mh-e identity menu"
      (append
       '("Identity")
       ;; Dynamically render :type corresponding to `mh-identity-list'
       ;; e.g.:
       ;;  ["home" (mh-insert-identity "home")
       ;;   :style radio :active (not (equal mh-identity-local "home"))
       ;;   :selected (equal mh-identity-local "home")]
       (mapcar (function
                (lambda (arg)
                  `[,arg  (mh-insert-identity ,arg) :style radio
                          :active (not (equal mh-identity-local ,arg))
                          :selected (equal mh-identity-local ,arg)]))
               (mapcar 'car mh-identity-list))
       '("--"
         ["none" (mh-insert-identity "none") mh-identity-local]
         ["Set Default for Session"
          (setq mh-identity-default mh-identity-local) t]
         ["Save as Default"
          (customize-save-variable
           'mh-identity-default mh-identity-local) t]
         )))))

;;;###mh-autoload
(defun mh-identity-list-set (symbol value)
  "Update the `mh-identity-list' variable, and rebuild the menu.
Sets the default for SYMBOL (e.g. `mh-identity-list') to VALUE (as set in
customization).  This is called after 'customize is used to alter
`mh-identity-list'."
  (set-default symbol value)
  (mh-identity-make-menu))

(defvar mh-identity-local nil
  "Buffer-local variable holding the identity currently in use.")
(make-variable-buffer-local 'mh-identity-local)

(defun mh-header-field-delete (field value-only)
  "Delete FIELD in the mail header, or only its value if VALUE-ONLY is t.
Return t if anything is deleted."
  (when (mh-goto-header-field field)
    (if (not value-only)
        (beginning-of-line)
      (forward-char))
    (delete-region (point)
                   (progn (mh-header-field-end)
                          (if (not value-only) (forward-char 1))
                          (point)))
    t))

(defvar mh-identity-signature-start nil
  "Marker for the beginning of a signature inserted by `mh-insert-identity'.")
(defvar mh-identity-signature-end nil
  "Marker for the end of a signature inserted by `mh-insert-identity'.")

;;;###mh-autoload
(defun mh-insert-identity (identity)
  "Insert proper fields for given IDENTITY.
Edit the `mh-identity-list' variable to define identity."
  (interactive
   (list (completing-read
          "Identity: "
          (if mh-identity-local
              (cons '("none")
                    (mapcar 'list (mapcar 'car mh-identity-list)))
            (mapcar 'list (mapcar 'car mh-identity-list)))
          nil t)))
  (save-excursion
    ;;First remove old settings, if any.
    (when mh-identity-local
      (let ((pers-list (cadr (assoc mh-identity-local mh-identity-list))))
        (while pers-list
          (let ((field (concat (caar pers-list) ":")))
            (cond
             ((string-equal "signature:" field)
              (when (and (boundp 'mh-identity-signature-start)
                         (markerp mh-identity-signature-start))
                (goto-char mh-identity-signature-start)
                (forward-char -1)
                (delete-region (point) mh-identity-signature-end)))
             ((mh-header-field-delete field nil))))
          (setq pers-list (cdr pers-list)))))
    ;; Then insert the replacement
    (when (not (equal "none" identity))
      (let ((pers-list (cadr (assoc identity mh-identity-list))))
        (while pers-list
          (let ((field (concat (caar pers-list) ":"))
                (value (cdar pers-list)))
            (cond
             ;; No value, remove field
             ((or (not value)
                  (string= value ""))
              (mh-header-field-delete field nil))
             ;; Existing field, replace
             ((mh-header-field-delete field t)
              (insert value))
             ;; Handle "signature" special case. Insert file or call function.
             ((and (string-equal "signature:" field)
                   (or (and (stringp value)
                            (file-readable-p value))
                       (fboundp value)))
              (goto-char (point-max))
              (if (not (looking-at "^$"))
                  (insert "\n"))
              (insert "\n")
              (save-restriction
                (narrow-to-region (point) (point))
                (set (make-local-variable 'mh-identity-signature-start)
                     (make-marker))
                (set-marker mh-identity-signature-start (point))
                (cond
                 ;; If MIME composition done, insert signature at the end as
                 ;; an inline MIME part.
                 ((mh-mhn-directive-present-p)
                  (insert "#\n" "Content-Description: Signature\n"))
                 ((mh-mml-directive-present-p)
                  (mml-insert-tag 'part 'type "text/plain"
                                  'disposition "inline"
                                  'description "Signature")))
                (if (stringp value)
                    (insert-file-contents value)
                  (funcall value))
                (goto-char (point-min))
                (when (not (re-search-forward "^--" nil t))
                  (cond ((mh-mhn-directive-present-p)
                         (forward-line 2))
                        ((mh-mml-directive-present-p)
                         (forward-line 1)))
                  (insert "-- \n"))
                (set (make-local-variable 'mh-identity-signature-end)
                     (make-marker))
                (set-marker mh-identity-signature-end (point-max))))
             ;; Handle "From" field differently, adding it at the beginning.
             ((string-equal "From:" field)
              (goto-char (point-min))
              (insert "From: " value "\n"))
             ;; Skip empty signature (Can't remove what we don't know)
             ((string-equal "signature:" field))
             ;; Other field, add at end
             (t                         ;Otherwise, add the end.
              (goto-char (point-min))
              (mh-goto-header-end 0)
              (mh-insert-fields field value))))
          (setq pers-list (cdr pers-list))))))
  ;; Remember what is in use in this buffer
  (if (equal "none" identity)
      (setq mh-identity-local nil)
    (setq mh-identity-local identity)))

(provide 'mh-identity)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 07d66ef6-8726-4ac6-9ecf-e566cd5bfb45
;;; mh-identity.el ends here
