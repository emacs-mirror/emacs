;;; newsticker-testsuite.el --- Test suite for newsticker.

;; Copyright (C) 2003-2018 Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Keywords:    News, RSS, Atom

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

(require 'ert)
(require 'newsticker)

;; ======================================================================
;; Tests for newsticker-backend
;; ======================================================================
(ert-deftest newsticker--guid ()
  "Test for `newsticker--guid-*'.
Signals an error if something goes wrong."
  (should (string= "blah" (newsticker--guid-to-string "blah")))
  (should (string= "myguid" (newsticker--guid '("title1" "description1" "link1"
                                                nil 'new 42 nil nil
                                                ((guid () "myguid")))))))

(ert-deftest newsticker--cache-contains ()
  "Test for `newsticker--cache-contains'."
  (let ((newsticker--cache '((feed1
                              ("title1" "description1" "link1" nil 'new 42
                               nil nil ((guid () "myguid")))))))
    (newsticker--guid-to-string
     (assoc 'guid (newsticker--extra '("title1" "description1"
                                       "link1" nil 'new 42 nil nil
                                       ((guid "myguid"))))))
    (should (newsticker--cache-contains newsticker--cache 'feed1 "WRONGTITLE"
                                        "description1" "link1" 'new "myguid"))
    (should (not (newsticker--cache-contains newsticker--cache 'feed1 "title1"
                                             "description1" "link1" 'new
                                             "WRONG GUID")))
    (should (newsticker--cache-contains newsticker--cache 'feed1 "title1"
                                        "description1" "link1" 'new "myguid")))
  (let ((newsticker--cache '((feed1
                              ("title1" "description1" "link1" nil 'new 42
                               nil nil ((guid () "myguid1")))
                              ("title1" "description1" "link1" nil 'new 42
                               nil nil ((guid () "myguid2")))))))
    (should (not (newsticker--cache-contains newsticker--cache 'feed1 "title1"
                                             "description1" "link1" 'new
                                             "myguid")))
    (should (string= "myguid1"
                     (newsticker--guid (newsticker--cache-contains
                                        newsticker--cache 'feed1 "title1"
                                        "description1" "link1" 'new
                                        "myguid1"))))
    (should (string= "myguid2"
                     (newsticker--guid (newsticker--cache-contains
                                        newsticker--cache 'feed1 "title1"
                                        "description1" "link1" 'new
                                        "myguid2"))))))

(defun newsticker-tests--decode-iso8601-date (input expected)
  "Actually test `newsticker--decode-iso8601-date'.
Apply to INPUT and compare with EXPECTED."
  (let ((result (format-time-string "%Y-%m-%dT%H:%M:%S"
                                    (newsticker--decode-iso8601-date input)
                                    t)))
    (should (string= result expected))))

(ert-deftest newsticker--decode-iso8601-date ()
  "Test `newsticker--decode-iso8601-date'."
  (newsticker-tests--decode-iso8601-date "2004"
                                         "2004-01-01T00:00:00")
  (newsticker-tests--decode-iso8601-date "2004-09"
                                         "2004-09-01T00:00:00")
  (newsticker-tests--decode-iso8601-date "2004-09-17"
                                         "2004-09-17T00:00:00")
  (newsticker-tests--decode-iso8601-date "2004-09-17T05:09"
                                         "2004-09-17T05:09:00")
  (newsticker-tests--decode-iso8601-date "2004-09-17T05:09:49"
                                         "2004-09-17T05:09:49")
  (newsticker-tests--decode-iso8601-date "2004-09-17T05:09:49.123"
                                         "2004-09-17T05:09:49")
  (newsticker-tests--decode-iso8601-date "2004-09-17T05:09+01:00"
                                         "2004-09-17T04:09:00")
  (newsticker-tests--decode-iso8601-date "2004-09-17T05:09-02:00"
                                         "2004-09-17T07:09:00"))

(defun newsticker--do-test--decode-rfc822-date (input expected)
  "Actually test `newsticker--decode-rfc822-date'.
Apply to INPUT and compare with EXPECTED."
  (let ((result (format-time-string "%Y-%m-%dT%H:%M:%S"
                                    (newsticker--decode-rfc822-date input)
                                    t)))
    (should (string= result expected))))

(ert-deftest newsticker--decode-rfc822-date ()
  "Test `newsticker--decode-rfc822-date'."
  (newsticker--do-test--decode-rfc822-date "Mon, 10 Mar 2008 19:27:52 +0100"
                                           "2008-03-10T18:27:52")
  ;;(format-time-string "%d.%m.%y, %H:%M %T%z"
  ;;(newsticker--decode-rfc822-date "Mon, 10 Mar 2008 19:27:52 +0200"))

  (newsticker--do-test--decode-rfc822-date "Mon, 10 Mar 2008 19:27:52"
                                           "2008-03-10T19:27:52")
  (newsticker--do-test--decode-rfc822-date "Mon, 10 Mar 2008 19:27"
                                           "2008-03-10T19:27:00")
  (newsticker--do-test--decode-rfc822-date "10 Mar 2008 19:27"
                                           "2008-03-10T19:27:00")
  (newsticker--do-test--decode-rfc822-date "Mon, 10 Mar 2008"
                                           "2008-03-10T00:00:00")
  (newsticker--do-test--decode-rfc822-date "10 Mar 2008"
                                           "2008-03-10T00:00:00")
  (newsticker--do-test--decode-rfc822-date "Sat, 01 Dec 2007 00:05:00 +0100"
                                           "2007-11-30T23:05:00")
  (newsticker--do-test--decode-rfc822-date "Sun, 30 Dec 2007 18:58:13 +0100"
                                           "2007-12-30T17:58:13"))

;; ======================================================================
;; Tests for newsticker-treeview
;; ======================================================================
(ert-deftest newsticker--group-manage-orphan-feeds ()
  "Test `newsticker--group-manage-orphan-feeds'.
Signals an error if something goes wrong."
  (let ((newsticker-groups '("Feeds"))
        (newsticker-url-list-defaults nil)
        (newsticker-url-list '(("feed1") ("feed2") ("feed3"))))
    (newsticker--group-manage-orphan-feeds)
    (should (equal '("Feeds" "feed3" "feed2" "feed1")
                   newsticker-groups))))

(ert-deftest newsticker--group-find-parent-group ()
  "Test `newsticker--group-find-parent-group'."
  (let ((newsticker-groups '("g1" "f1a" ("g2" "f2" ("g3" "f3a" "f3b")) "f1b")))
    ;; feeds
    (should (equal "g1" (car (newsticker--group-find-parent-group "f1a"))))
    (should (equal "g1" (car (newsticker--group-find-parent-group "f1b"))))
    (should (equal "g2" (car (newsticker--group-find-parent-group "f2"))))
    (should (equal "g3" (car (newsticker--group-find-parent-group "f3b"))))
    ;; groups
    (should (equal "g1" (car (newsticker--group-find-parent-group "g2"))))
    (should (equal "g2" (car (newsticker--group-find-parent-group "g3"))))))

(ert-deftest newsticker--group-do-rename-group ()
  "Test `newsticker--group-do-rename-group'."
  (let ((newsticker-groups '("g1" "f1a" ("g2" "f2" ("g3" "f3a" "f3b")) "f1b")))
    (should (equal '("g1" "f1a" ("h2" "f2" ("g3" "f3a" "f3b")) "f1b")
                   (newsticker--group-do-rename-group "g2" "h2")))
    ))


(provide 'newsticker-tests)

;;; newsticker-tests.el ends here
