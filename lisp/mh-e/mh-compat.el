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

;; See also mh-gnus.el for compatibility macros used to span different
;; versions of Gnus.

;; Macros are listed alphabetically.

(unless (fboundp 'assoc-string)
  (defsubst assoc-string (key list case-fold)
    "Like `assoc' but specifically for strings.
Case is ignored if CASE-FOLD is non-nil.
This function added by MH-E for Emacs versions that lack
`assoc-string', introduced in Emacs 22."
    (if case-fold
        (assoc-ignore-case key list)
      (assoc key list))))

(defmacro mh-display-completion-list (completions &optional common-substring)
  "Display the list of COMPLETIONS.
Calls `display-completion-list' correctly in older environments.
Versions of Emacs prior to version 22 lacked a COMMON-SUBSTRING
argument which is used to highlight the next possible character you
can enter in the current list of completions."
  (if (< emacs-major-version 22)
      `(display-completion-list ,completions)
    `(display-completion-list ,completions ,common-substring)))

(provide 'mh-compat)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;; arch-tag: 577b0eab-a5cd-45e1-8d9f-c1a426f4d73c
;;; mh-compat.el ends here
