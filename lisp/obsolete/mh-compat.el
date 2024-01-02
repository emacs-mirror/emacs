;;; mh-compat.el --- make MH-E compatible with various versions of Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2006-2024 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el
;; Obsolete-since: 29.1

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

;;; Code:

;; This is a good place to gather code that is used for compatibility
;; between different versions of Emacs. Please document which versions
;; of Emacs that the defsubst, defalias, or defmacro applies. That
;; way, it's easy to occasionally go through this file and see which
;; macros we can retire.

;; Please use mh-gnus.el when providing compatibility with different
;; versions of Gnus.

;; Items are listed alphabetically.

(eval-when-compile (require 'mh-acros))

(define-obsolete-function-alias 'mh-require #'require "29.1")
(define-obsolete-function-alias 'mh-assoc-string #'assoc-string "29.1")
(define-obsolete-function-alias 'mh-cancel-timer #'cancel-timer "29.1")

(define-obsolete-function-alias 'mh-display-color-cells
  #'display-color-cells "29.1")

(defmacro mh-display-completion-list (completions &optional common-substring)
  "Display the list of COMPLETIONS.
See documentation for `display-completion-list' for a description of the
arguments COMPLETIONS.
The optional argument COMMON-SUBSTRING, if non-nil, should be a string
specifying a common substring for adding the faces
`completions-first-difference' and `completions-common-part' to
the completions."
  (declare (obsolete nil "29.1"))
  `(display-completion-list
    (completion-hilit-commonality ,completions
                                  ,(length common-substring) nil)))

(define-obsolete-function-alias 'mh-face-foreground
  #'face-foreground "29.1")

(define-obsolete-function-alias 'mh-face-background
  #'face-background "29.1")

(define-obsolete-function-alias 'mh-font-lock-add-keywords
  #'font-lock-add-keywords "29.1")

;; Not preloaded in without-x builds.
(declare-function image-load-path-for-library "image")
(define-obsolete-function-alias 'mh-image-load-path-for-library
  #'image-load-path-for-library "29.1")

;; Not preloaded in without-x builds.
(declare-function image-search-load-path "image")
(define-obsolete-function-alias 'mh-image-search-load-path
  #'image-search-load-path "29.1")

(define-obsolete-function-alias 'mh-line-beginning-position
  #'line-beginning-position "29.1")

(define-obsolete-function-alias 'mh-line-end-position
  #'line-end-position "29.1")

(require 'mailabbrev)
(define-obsolete-function-alias 'mh-mail-abbrev-make-syntax-table
  #'mail-abbrev-make-syntax-table "29.1")

(define-obsolete-function-alias 'mh-define-obsolete-variable-alias
  #'define-obsolete-variable-alias "29.1")

(define-obsolete-function-alias 'mh-make-obsolete-variable
  #'make-obsolete-variable "29.1")

(define-obsolete-function-alias 'mh-match-string-no-properties
  #'match-string-no-properties "29.1")

(define-obsolete-function-alias 'mh-replace-regexp-in-string
  #'replace-regexp-in-string "29.1")

(define-obsolete-function-alias 'mh-test-completion
  #'test-completion "29.1")

(defconst mh-url-unreserved-chars
  '(
    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
       ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")
(make-obsolete-variable 'mh-url-unreserved-chars 'url-unreserved-chars "29.1")

(define-obsolete-function-alias 'mh-url-hexify-string
  #'url-hexify-string "29.1")

(define-obsolete-function-alias 'mh-view-mode-enter
  #'view-mode-enter "29.1")

(define-obsolete-function-alias 'mh-window-full-height-p
  #'window-full-height-p "29.1")

(defmacro mh-write-file-functions ()
  "Return `write-file-functions'."
  (declare (obsolete nil "29.1"))
  ''write-file-functions)

(provide 'mh-compat)

;; Local Variables:
;; sentence-end-double-space: nil
;; End:

;;; mh-compat.el ends here
