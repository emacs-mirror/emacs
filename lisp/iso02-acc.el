;;; iso02-acc.el --- electric accent keys for Eastern Europe (ISO latin2)

;; Copyright (C) 1995 Free Software Foundation, Inc.

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

;;; Code:

(defvar iso-accents-list
  '(((?' ?A) ?\301)
    ((?' ?C) ?\306) 
    ((?' ?D) ?\320)
    ((?' ?E) ?\311)
    ((?' ?I) ?\315)
    ((?' ?L) ?\305)
    ((?' ?N) ?\321)
    ((?' ?O) ?\323)
    ((?' ?R) ?\300)
    ((?' ?S) ?\246)
    ((?' ?U) ?\332)
    ((?' ?Y) ?\335)
    ((?' ?Z) ?\254)
    ((?' ?a) ?\341)
    ((?' ?c) ?\346)
    ((?' ?d) ?\360)  
    ((?' ?e) ?\351)
    ((?' ?i) ?\355)
    ((?' ?l) ?\345)
    ((?' ?n) ?\361)
    ((?' ?o) ?\363)
    ((?' ?r) ?\340)
    ((?' ?s) ?\266)
    ((?' ?u) ?\372)
    ((?' ?y) ?\375)
    ((?' ?z) ?\274)
    ((?' ?') ?\264)
    ((?' ? ) ?')
    ((?` ?A) ?\241)
    ((?` ?C) ?\307)
    ((?` ?E) ?\312)
    ((?` ?L) ?\243)
    ((?` ?S) ?\252)
    ((?` ?T) ?\336)
    ((?` ?Z) ?\257)
    ((?` ?a) ?\261)
    ((?` ?l) ?\263)
    ((?` ?c) ?\347)
    ((?` ?e) ?\352)
    ((?` ?s) ?\272)
    ((?` ?t) ?\376)
    ((?` ?z) ?\277)
    ((?` ? ) ?`)
    ((?` ?`) ?\252)
    ((?` ?.) ?\377)
    ((?^ ?A) ?\302)
    ((?^ ?O) ?\324)
    ((?^ ?a) ?\342)
    ((?^ ?o) ?\364)
    ((?^ ? ) ?^)
    ((?^ ?^) ?^)		; no special code?
    ((?\" ?A) ?\304)
    ((?\" ?E) ?\313)
    ((?\" ?O) ?\326)
    ((?\" ?U) ?\334)
    ((?\" ?a) ?\344)
    ((?\" ?e) ?\353)
    ((?\" ?o) ?\366)
    ((?\" ?s) ?\337)
    ((?\" ?u) ?\374)
    ((?\" ? ) ?\")
    ((?\" ?\") ?\250)
    ((?\~ ?A) ?\303)
    ((?\~ ?C) ?\310)
    ((?\~ ?D) ?\317)
    ((?\~ ?L) ?\245)
    ((?\~ ?N) ?\322)
    ((?\~ ?O) ?\325)
    ((?\~ ?R) ?\330)
    ((?\~ ?S) ?\251)
    ((?\~ ?T) ?\253)
    ((?\~ ?U) ?\333)
    ((?\~ ?Z) ?\256)
    ((?\~ ?a) ?\323)
    ((?\~ ?c) ?\350)
    ((?\~ ?d) ?\357)
    ((?\~ ?l) ?\265)
    ((?\~ ?n) ?\362)
    ((?\~ ?o) ?\365)
    ((?\~ ?r) ?\370)
    ((?\~ ?s) ?\271)
    ((?\~ ?t) ?\273)
    ((?\~ ?u) ?\373)
    ((?\~ ?z) ?\276)
    ((?\~ ?\ ) ?\~)
    ((?\~ ?v) ?\242) ;; v accent
    ((?\~ ?\~) ?\242) ;; v accent
    ((?\~ ?\.) ?\270) ;; cedilla accent
    )
  "Association list for ISO latin-2 accent combinations.")

(defvar iso-accents-enable '(?' ?` ?^ ?\" ?~)
  "*List of accent keys that become prefixes in ISO Accents mode.
The default is (?' ?` ?^ ?\" ?~), which contains all the supported
accent keys.  For certain languages, you might want to remove some of
those characters that are not actually used.")

(require 'iso-acc)

;;; iso02-acc.el ends here
