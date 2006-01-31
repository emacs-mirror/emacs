;;; mh-compat.el --- make MH-E compatibile with various versions of Emacs

;; Copyright (C) 2006 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
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

;;; Change Log:

;;; Code:

;; This is a good place to gather code that is used for compatibility
;; between different versions of Emacs. Please document which versions
;; of Emacs that the defsubst, defalias, or defmacro applies. That
;; way, it's easy to occasionally go through this file and see which
;; macros we can retire.

;; Please use mh-gnus.el when providing compatibility with different
;; versions of Gnus and mh-xemacs.el for compatibility with XEmacs.

;; Items are listed alphabetically.

(mh-defun-compat mh-assoc-string assoc-string (key list case-fold)
  "Like `assoc' but specifically for strings.
Case is ignored if CASE-FOLD is non-nil.
This function added by MH-E for Emacs versions that lack
`assoc-string', introduced in Emacs 22."
  (if case-fold
      (assoc-ignore-case key list)
    (assoc key list)))

(require 'mailabbrev nil t)
(mh-defun-compat mh-mail-abbrev-make-syntax-table
  mail-abbrev-make-syntax-table ()
  "Emacs 21 and XEmacs don't have this function."
  nil)

(defmacro mh-display-completion-list (completions &optional common-substring)
  "Display the list of COMPLETIONS.
See documentation for `display-completion-list' for a description of the
arguments COMPLETIONS and perhaps COMMON-SUBSTRING.
This macro added by MH-E for Emacs versions that lack a
COMMON-SUBSTRING argument, introduced in Emacs 22."
  (if (< emacs-major-version 22)
      `(display-completion-list ,completions)
    `(display-completion-list ,completions ,common-substring)))

(defmacro mh-face-foreground (face &optional frame inherit)
  "Return the foreground color name of FACE, or nil if unspecified.
See documentation for `face-foreground' for a description of the
arguments FACE, FRAME, and perhaps INHERIT.
This macro added by MH-E for Emacs versions that lack an INHERIT
argument, introduced in Emacs 22."
  (if (< emacs-major-version 22)
      `(face-foreground ,face ,frame)
    `(face-foreground ,face ,frame ,inherit)))

(defmacro mh-face-background (face &optional frame inherit)
  "Return the background color name of face, or nil if unspecified.
See documentation for `back-foreground' for a description of the
arguments FACE, FRAME, and INHERIT.
This macro added by MH-E for Emacs versions that lack an INHERIT
argument, introduced in Emacs 22."
  (if (< emacs-major-version 22)
      `(face-background ,face ,frame)
    `(face-background ,face ,frame ,inherit)))

;; Copy of constant from url-util.el in Emacs 22; needed by Emacs 21.
(if (not (boundp 'url-unreserved-chars))
    (defconst mh-url-unresrved-chars
      '(
        ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
        ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
        ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
        ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
      "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396."))

(mh-defun-compat mh-url-hexify-string url-hexify-string (str)
  "Escape characters in a string.
This is a copy of `url-hexify-string' from url-util.el in Emacs
22; needed by Emacs 21."
  (mapconcat
   (lambda (char)
     ;; Fixme: use a char table instead.
     (if (not (memq char mh-url-unreserved-chars))
         (if (> char 255)
               (error "Hexifying multibyte character %s" str)
           (format "%%%02X" char))
       (char-to-string char)))
   str ""))

(defmacro mh-write-file-functions ()
  "Return `write-file-functions' if it exists.
Otherwise return `local-write-file-hooks'.
This macro exists purely for compatibility. The former symbol is used
in Emacs 22 onward while the latter is used in previous versions and
XEmacs."
  (if (boundp 'write-file-functions)
      ''write-file-functions            ;Emacs 22 on
    ''local-write-file-hooks))          ;XEmacs

(provide 'mh-compat)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;; arch-tag: 577b0eab-a5cd-45e1-8d9f-c1a426f4d73c
;;; mh-compat.el ends here
