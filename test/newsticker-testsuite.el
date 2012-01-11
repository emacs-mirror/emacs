;;; newsticker-testsuite.el --- Test suite for newsticker.

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
;;   Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newsticker-testsuite.el
;; URL:         http://www.nongnu.org/newsticker
;; Keywords:    News, RSS, Atom
;; Time-stamp:  "14. Juni 2008, 12:09:39 (ulf)"

;; ======================================================================

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; ======================================================================

;;; Commentary:

;;; Code:

(require 'cl)                           ; assert

;; ======================================================================
;; Entry point
;; ======================================================================
(defun newsticker--testsuite ()
  "Unit test for newsticker.
Subtests signal errors if something goes wrong."
  (interactive)
  (newsticker--test--guid)
  (newsticker--test--cache-contains)
  (newsticker--test--decode-iso8601-date)
  (newsticker--test--decode-rfc822-date)
  (newsticker--test--group-manage-orphan-feeds)
  (message "All tests passed successfully."))

;; ======================================================================
;; Tests for newsticker-backend
;; ======================================================================
(defun newsticker--test--guid ()
  "Test `newsticker-guid-*'.
Signals an error if something goes wrong."
  (assert (string= "blah" (newsticker--guid-to-string "blah")))
  (assert (string= "myguid" (newsticker--guid '("title1" "description1" "link1"
                                                nil 'new 42 nil nil
                                                ((guid () "myguid")))))))

(defun newsticker--test--cache-contains ()
  "Test `newsticker--test--cache-contains'.
Signals an error if something goes wrong."
  (let ((newsticker--cache '((feed1
                              ("title1" "description1" "link1" nil 'new 42
                               nil nil ((guid () "myguid")))))))
    (newsticker--guid-to-string
     (assoc 'guid (newsticker--extra '("title1" "description1"
                                       "link1" nil 'new 42 nil nil
                                       ((guid "myguid"))))))
    (assert (newsticker--cache-contains newsticker--cache 'feed1 "WRONGTITLE"
                                        "description1" "link1" 'new "myguid"))
    (assert (not (newsticker--cache-contains newsticker--cache 'feed1 "title1"
                                             "description1" "link1" 'new
                                             "WRONG GUID")))
    (assert (newsticker--cache-contains newsticker--cache 'feed1 "title1"
                                        "description1" "link1" 'new "myguid")))
  (let ((newsticker--cache '((feed1
                              ("title1" "description1" "link1" nil 'new 42
                               nil nil ((guid () "myguid1")))
                              ("title1" "description1" "link1" nil 'new 42
                               nil nil ((guid () "myguid2")))))))
    (assert (not (newsticker--cache-contains newsticker--cache 'feed1 "title1"
                                             "description1" "link1" 'new
                                             "myguid")))
    (assert (string= "myguid1"
                     (newsticker--guid (newsticker--cache-contains
                                        newsticker--cache 'feed1 "title1"
                                        "description1" "link1" 'new
                                        "myguid1"))))
    (assert (string= "myguid2"
                     (newsticker--guid (newsticker--cache-contains
                                        newsticker--cache 'feed1 "title1"
                                        "description1" "link1" 'new
                                        "myguid2"))))))

(defun newsticker--do-test--decode-iso8601-date (input expected)
  "Actually test `newsticker--decode-iso8601-date'.
Signals an error if iso8601-encoded INPUT does not match EXPECTED."
  (let ((result (format-time-string "%Y-%m-%dT%H:%M:%S"
                                    (newsticker--decode-iso8601-date input)
                                    t)))
    (assert (string= result expected)
            nil "Error decoding '%s': found '%s' but expected '%s'."
            input result expected)))

(defun newsticker--test--decode-iso8601-date ()
  "Test `newsticker--decode-iso8601-date'."
  (newsticker--decode-iso8601-date "2004-09-17T05:09:49+00:00")
  (newsticker--decode-iso8601-date "2004-09-17T05:09+00:00")
  (newsticker--decode-iso8601-date "2004-09-17T05:09:49")
  (newsticker--decode-iso8601-date "2004-09-17T05:09")
  (newsticker--decode-iso8601-date "2004-09-17")
  (newsticker--decode-iso8601-date "2004-09")
  (newsticker--do-test--decode-iso8601-date "2004"
                                            "2004-01-01T00:00:00")
  (newsticker--do-test--decode-iso8601-date "2004-09"
                                            "2004-09-01T00:00:00")
  (newsticker--do-test--decode-iso8601-date "2004-09-17"
                                            "2004-09-17T00:00:00")
  (newsticker--do-test--decode-iso8601-date "2004-09-17T05:09"
                                            "2004-09-17T05:09:00")
  (newsticker--do-test--decode-iso8601-date "2004-09-17T05:09:49"
                                            "2004-09-17T05:09:49")
  (newsticker--do-test--decode-iso8601-date "2004-09-17T05:09:49.123"
                                            "2004-09-17T05:09:49")
  (newsticker--do-test--decode-iso8601-date "2004-09-17T05:09+01:00"
                                            "2004-09-17T04:09:00")
  (newsticker--do-test--decode-iso8601-date "2004-09-17T05:09-02:00"
                                            "2004-09-17T07:09:00"))

(defun newsticker--do-test--decode-rfc822-date (input expected)
  "Actually test `newsticker--decode-rfc822-date'.
Signals an error if rfc822-encoded INPUT does not match EXPECTED."
  (let ((result (format-time-string "%Y-%m-%dT%H:%M:%S"
                                    (newsticker--decode-rfc822-date input)
                                    t)))
    (assert (string= result expected)
            nil "Error decoding '%s': found '%s' but expected '%s'."
            input result expected)))

(defun newsticker--test--decode-rfc822-date ()
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
(defun newsticker--test--group-manage-orphan-feeds ()
  "Test `newsticker--group-manage-orphan-feeds'.
Signals an error if something goes wrong."
  (let ((newsticker-groups '("Feeds"))
        (newsticker-url-list-defaults nil)
        (newsticker-url-list '(("feed1") ("feed2") ("feed3"))))
    (newsticker--group-manage-orphan-feeds)
    (assert (equal '("Feeds" "feed3" "feed2" "feed1")
                   newsticker-groups))))

(provide 'newsticker-testsuite)

;; arch-tag: e6c09af2-cc7a-4373-8f5f-9c36699ec34c
;;; newsticker-testsuite.el ends here
