;;; gnus-test-headers.el --- Tests for Gnus header-related functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>

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

;; The tests her are for

;;; Code:

(require 'ert)
(require 'gnus-sum)

(defconst gnus-headers-test-data
  '([2 "Re: [Emacs-devel] Emacs move" "Dave Love <d.love@dl.ac.uk>"
       "Thu, 14 Sep 2000 11:10:46 +0100"
       "<200009141010.LAA26351@djlvig.dl.ac.uk>"
       "<20000913175943.A26093@sparky.nisa.net>"
       1882 16 "nnmaildir mails:2"
       ((To . "Jeff Bailey <jbailey@nisa.net>")
        (Cc . "emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [3 "Re: [Emacs-devel] Emacs move" "Sam Steingold <sds@gnu.org>"
       "14 Sep 2000 10:21:56 -0400" "<upum7xddn.fsf@xchange.com>"
       "<20000913175943.A26093@sparky.nisa.net>"
       2991 50 "nnmaildir mails:3"
       ((To . "Jeff Bailey <jbailey@nisa.net>")
        (Cc . "emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [4 "Re: [Emacs-devel] Emacs move" "Jeff Bailey <jbailey@nisa.net>"
       "Thu, 14 Sep 2000 09:14:47 -0700"
       "<20000914091447.G4827@sparky.nisa.net>"
       "<20000913175943.A26093@sparky.nisa.net> <upum7xddn.fsf@xchange.com>"
       1780 15 "nnmaildir mails:4"
       ((To . "sds@gnu.org, Jeff Bailey <jbailey@nisa.net>")
        (Cc . "emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [5 "Re: [Emacs-devel] Emacs move" "Dave Love <d.love@dl.ac.uk>"
       "Thu, 14 Sep 2000 18:24:36 +0100"
       "<200009141724.SAA26807@djlvig.dl.ac.uk>"
       "<20000913175943.A26093@sparky.nisa.net>"
       1343 9 "nnmaildir mails:5"
       ((To . "Jeff Bailey <jbailey@nisa.net>")
        (Cc . "emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [6 "Re: [Emacs-devel] Emacs move" "Karl Fogel <kfogel@galois.collab.net>"
       "14 Sep 2000 10:37:35 -0500" "<87em2nyog0.fsf@galois.collab.net>"
       "<20000913175943.A26093@sparky.nisa.net> <200009141724.SAA26807@djlvig.dl.ac.uk>"
       3740 124 "nnmaildir mails:6"
       ((To . "Dave Love <d.love@dl.ac.uk>")
        (Cc . "Jeff Bailey <jbailey@nisa.net>, emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [7 "Re: [Emacs-devel] Emacs move" "Jeff Bailey <jbailey@nisa.net>"
       "Thu, 14 Sep 2000 10:55:12 -0700"
       "<20000914105512.A29291@sparky.nisa.net>"
       "<20000913175943.A26093@sparky.nisa.net> <200009141724.SAA26807@djlvig.dl.ac.uk> <87em2nyog0.fsf@galois.collab.net>"
       1687 16 "nnmaildir mails:7"
       ((To . "kfogel@red-bean.com, Dave Love <d.love@dl.ac.uk>")
        (Cc . "Jeff Bailey <jbailey@nisa.net>, emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [8 "Re: [Emacs-devel] Emacs move" "John Wiegley <johnw@gnu.org>"
       "Thu, 14 Sep 2000 12:19:01 -0700"
       "<200009141919.MAA05085@localhost.localdomain>"
       "<20000913175943.A26093@sparky.nisa.net>"
       1978 27 "nnmaildir mails:8"
       ((To . "emacs-devel@gnu.org"))]
    [9 "Re: [Emacs-devel] Emacs move"
       "\"Robert J. Chassell\" <bob@rattlesnake.com>"
       "Thu, 14 Sep 2000 07:33:15 -0400 (EDT)"
       "<m13ZXGV-000BCgC@megalith.rattlesnake.com>"
       "<20000913175943.A26093@sparky.nisa.net>"
       3046 72 "nnmaildir mails:9"
       ((To . "jbailey@nisa.net")
        (Cc . "emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [10 "Re: [Emacs-devel] Emacs move"
        "wmperry@aventail.com (William M. Perry)"
        "14 Sep 2000 09:10:25 -0500"
        "<86g0n3f4j2.fsf@megalith.bp.aventail.com>"
        "<20000913175943.A26093@sparky.nisa.net> <m13ZXGV-000BCgC@megalith.rattlesnake.com>"
        3104 44 "nnmaildir mails:10"
        ((To . "bob@rattlesnake.com")
         (Cc . "jbailey@nisa.net, emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [11 "Re: [Emacs-devel] Emacs move" "Gerd Moellmann <gerd@gnu.org>"
        "Thu, 14 Sep 2000 21:51:05 +0200 (CEST)"
        "<200009141951.VAA06005@gerd.segv.de>"
        "<20000913175943.A26093@sparky.nisa.net> <m13ZXGV-000BCgC@megalith.rattlesnake.com> <86g0n3f4j2.fsf@megalith.bp.aventail.com>"
        1884 6 "nnmaildir mails:11"
        ((To . "wmvperry@aventail.com")
         (Cc . "bob@rattlesnake.com, jbailey@nisa.net, emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [12 "Re: [Emacs-devel] Emacs move" "Gerd Moellmann <gerd@gnu.org>"
        "Thu, 14 Sep 2000 21:49:03 +0200 (CEST)"
        "<200009141949.VAA05998@gerd.segv.de>"
        "<20000913175943.A26093@sparky.nisa.net> <m13ZXGV-000BCgC@megalith.rattlesnake.com>"
        2408 24 "nnmaildir mails:12"
        ((To . "bob@rattlesnake.com")
         (Cc . "jbailey@nisa.net, emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [13 "Re: [Emacs-devel] Emacs move"
        "\"Robert J. Chassell\" <bob@rattlesnake.com>"
        "Thu, 14 Sep 2000 17:50:01 -0400 (EDT)"
        "<m13ZgtN-000BD3C@megalith.rattlesnake.com>"
        "<20000913175943.A26093@sparky.nisa.net> <m13ZXGV-000BCgC@megalith.rattlesnake.com> <200009141949.VAA05998@gerd.segv.de>"
        1968 23 "nnmaildir mails:13"
        ((To . "gerd@gnu.org")
         (Cc . "bob@rattlesnake.com, jbailey@nisa.net, emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [14 "Re: [Emacs-devel] Emacs move" "Richard Stallman <rms@gnu.org>"
        "Fri, 15 Sep 2000 16:28:12 -0600 (MDT)"
        "<200009152228.QAA20526@wijiji.santafe.edu>"
        "<20000913175943.A26093@sparky.nisa.net> <m13ZXGV-000BCgC@megalith.rattlesnake.com>"
        1288 2 "nnmaildir mails:14"
        ((To . "jbailey@nisa.net, emacs-devel@gnu.org, cvs-hackers@gnu.org"))]
    [15 "[Emacs-devel] Emacs move" "Jeff Bailey <jbailey@nisa.net>"
        "Wed, 13 Sep 2000 17:59:43 -0700"
        "<20000913175943.A26093@sparky.nisa.net>" ""
        1661 26 "nnmaildir mails:15"
        ((To . "emacs-devel@gnu.org")
         (Cc . "cvs-hackers@gnu.org"))]
    [16 "Re: [Emacs-devel] Emacs move" "Jeff Bailey <jbailey@nisa.net>"
        "Fri, 15 Sep 2000 22:00:12 -0700"
        "<20000915220012.A3923@sparky.nisa.net>"
        "<20000913175943.A26093@sparky.nisa.net> <m13ZXGV-000BCgC@megalith.rattlesnake.com> <200009141949.VAA05998@gerd.segv.de> <m13ZgtN-000BD3C@megalith.rattlesnake.com>"
        2857 51 "nnmaildir mails:16"
        ((To . "bob@rattlesnake.com, gerd@gnu.org")
         (Cc . "jbailey@nisa.net, emacs-devel@gnu.org, cvs-hackers@gnu.org"))])
  "A pile of headers with potential interdependencies.")

(ert-deftest gnus-headers-make-dependency-table ()
  (let ((table (gnus-make-hashtable 20))
        (data (copy-sequence gnus-headers-test-data))
        ret)
    (dolist (h data)
      ;; `gnus-dependencies-add-header' returns nil if it fails to add
      ;; the header.
      (should (gnus-dependencies-add-header h table nil)))
    ;; Pick a value to test.
    (setq ret (gethash "<m13ZXGV-000BCgC@megalith.rattlesnake.com>"
                       table))
    ;; The message has three children.
    (should (= 3 (length (cdr ret))))
    ;; The first of those children has one child.
    (should (= 1 (length (cdr (nth 1 ret)))))))

(ert-deftest gnus-headers-loop-dependencies ()
  "Intentionally create a reference loop."
  (let ((table (gnus-make-hashtable 20))
        (data (copy-sequence gnus-headers-test-data))
        (parent-id "<200009141724.SAA26807@djlvig.dl.ac.uk>")
        (child-id "<87em2nyog0.fsf@galois.collab.net>")
        parent)
    (dolist (h data)
      (gnus-dependencies-add-header h table nil))

    (setq parent (gethash parent-id table))

    ;; Put the parent header in the child references of one of its own
    ;; children.  `gnus-thread-loop-p' only checks if there's a loop
    ;; between parent and immediate child, not parent and random
    ;; descendant.  At least, near as I can tell that's the case.

    (push (list (car parent)) (cdr (gethash child-id table)))

    (let ((gnus-newsgroup-dependencies table))
     (should
      (= 1                              ; 1 indicates an infloop.
         (gnus-thread-loop-p (car parent) (cadr parent)))))))

(provide 'gnus-test-headers)
;;; gnus-test-headers.el ends here
