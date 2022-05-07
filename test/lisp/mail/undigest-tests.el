;;; undigest-tests.el --- Tests for undigest.el  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'rmail)
(require 'undigest)

;;; Variables:
;; Some digests for testing.
(defvar rmail-rfc934-digest "From tester  Fri Jan 24 00:00:00 2022
From: Digester <digester@digester.com>
To: Undigester <undigester@undigester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Testing you

Testing the undigester.

------- Message sep

From: NN1 <nn1@nn1.com>
To: Digester <digester@digester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Message one

This is message one.

------- Message sep

From: NN2 <nn2@nn2.com>
To: Digester <digester@digester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Message two

This is message two.
"

  "RFC 934 digest.")

(defvar rmail-rfc1153-digest-strict "From tester  Fri Jan 24 00:00:00 2022
Date: ddd, dd mmm yy hh:mm:ss zzz
From: Digester <digester@digester.com>
To: Undigester <undigester@undigester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Testing you

Some mailing list information.

Today's Topics:

    1.  Message One Subject (Sender)
    2.  Message Two Subject (Sender)

----------------------------------------------------------------------

Date: ddd, dd mmm yy hh:mm:ss zzz
From: NN1 <nn1@nn1.com>
Subject: Message One Subject

This is message one.

------------------------------

Date: ddd, dd mmm yy hh:mm:ss zzz
From: NN2 <nn2@nn2.com>
Subject: Message Two Subject

This is message two.

------------------------------

End of Digest.
************************************
"
  "RFC 1153 strict style digest.")

(defvar rmail-rfc1153-digest-less-strict "From tester  Fri Jan 24 00:00:00 2022
From: Digester <digester@digester.com>
To: Undigester <undigester@undigester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Testing you

Some mailing list information.

Today's Topics:

    1.  Message One Subject (Sender)
    2.  Message Two Subject (Sender)

----------------------------------------------------------------------

Date: ddd, dd mmm yy hh:mm:ss zzz
From: NN1 <nn1@nn1.com>
Subject: Message One Subject

This is message one.

------------------------------

Date: ddd, dd mmm yy hh:mm:ss zzz
From: NN2 <nn2@nn2.com>
Subject: Message Two Subject

This is message two.

------------------------------

Subject: Digest Footer

End of Sbcl-help Digest, Vol 158, Issue 4
*****************************************
"
  "RFC 1153 style digest, with a Subject header.")

(defvar rmail-rfc1153-digest-sloppy "From tester  Fri Jan 24 00:00:00 2022
From: Digester <digester@digester.com>
To: Undigester <undigester@undigester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Testing you

Some mailing list information.

Today's Topics:

    1.  Message One Subject (Sender)
    2.  Message Two Subject (Sender)

----------------------------------------------------------------------

Date: ddd, dd mmm yy hh:mm:ss zzz
From: NN1 <nn1@nn1.com>
Subject: Message One Subject

This is message one.

------------------------------

Date: ddd, dd mmm yy hh:mm:ss zzz
From: NN2 <nn2@nn2.com>
Subject: Message Two Subject

This is message two.

------------------------------

Subject: Digest Footer

______________________________________________
Some blurb.

End of Digest.
************************************
"
  "RFC 1153 sloppy style digest.")

(defvar rmail-rfc1521-mime-digest "From tester  Fri Jan 24 00:00:00 2022
From: Digester <digester@digester.com>
To: Undigester <undigester@undigester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Test digest
MIME-Version: 1.0
Content-Type: multipart/digest; boundary=\"----- =_aaaaaaaaaa0\"

------- =_aaaaaaaaaa0
Content-Type: message/rfc822

From: NN1 <nn1@nn1.com>
To: Digester <digester@digester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Message one

Message one.

------- =_aaaaaaaaaa0

From: NN2 <nn2@nn2.com>
To: Digester <digester@digester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Message two

Message two.

------- =_aaaaaaaaaa0
"
  "RFC 1521 style MIME digest.")

(defvar rmail-multipart-mixed-digest
  "From tester  Fri Jan 24 00:00:00 2022
From: Digester <digester@digester.com>
To: Undigester <undigester@undigester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Test digest
Content-Type: multipart/mixed; boundary=\"===============2529375068597856000==\"
MIME-Version: 1.0

--===============2529375068597856000==
Content-Type: text/plain;
MIME-Version: 1.0
Content-Description: Today's Topics

Some message.

--===============2529375068597856000==
Content-Type: multipart/digest; boundary=\"===============6060050777038710134==\"
MIME-Version: 1.0

--===============6060050777038710134==
Content-Type: message/rfc822
MIME-Version: 1.0

From: NN1 <nn1@nn1.com>
To: Digester <digester@digester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Message one

Message one.

--===============6060050777038710134==
Content-Type: message/rfc822
MIME-Version: 1.0

From: NN2 <nn2@nn2.com>
To: Digester <digester@digester.com>
Date: ddd, dd mmm yy hh:mm:ss zzz
Subject: Message two

Message two.

--===============6060050777038710134==--

--===============2529375068597856000==
Content-Type: text/plain;
MIME-Version: 1.0
Content-Description: Digest Footer

The footer.

--===============2529375068597856000==--"
  "RFC 1521 digest inside a multipart/mixed message.")

;;; Utils:
(defun rmail-message-content (message)
  "Return the content of the message numbered MESSAGE."
  (rmail-show-message message)
  (let ((beg (rmail-msgbeg rmail-current-message))
        (end (rmail-msgend rmail-current-message)))
    (with-current-buffer rmail-view-buffer
      (save-excursion
	(goto-char beg)
	(search-forward "\n\n" end nil)
	(buffer-substring-no-properties (match-end 0) end)))))

;;; Tests:
(ert-deftest rmail-undigest-test-rfc934-digest ()
  "Test that we can undigest a RFC 934 digest."
  (let ((file (make-temp-file "undigest-test-")))
    (unwind-protect
        (with-temp-buffer
          (insert rmail-rfc934-digest)
          (write-region nil nil file)
          (rmail file)
          (undigestify-rmail-message)
          (should (= rmail-total-messages 4))
          (should (string= (rmail-message-content 2) "Testing the undigester.\n\n"))
          (should (string= (rmail-message-content 3) "This is message one.\n\n"))
          (should (string= (rmail-message-content 4) "This is message two.\n")))
      (delete-file file))))

(ert-deftest rmail-undigest-test-rfc1153-digest-strict ()
  "Test that we can undigest a strict RFC 1153 digest."
  :expected-result :failed
  (let ((file (make-temp-file "undigest-test-")))
    (unwind-protect
        (with-temp-buffer
          (insert rmail-rfc1153-digest-strict)
          (write-region nil nil file)
          (rmail file)
          (should
           (ignore-errors
             ;; This throws an error, because the Trailer is not recognized
             ;; as a valid RFC 822 (or later) message.
             (undigestify-rmail-message)
             (should (string= (rmail-message-content 2) "Testing the undigester.\n\n"))
             (should (string= (rmail-message-content 3) "This is message one.\n\n"))
             (should (string= (rmail-message-content 4) "This is message two.\n"))
             t)))
      (delete-file file))))

(ert-deftest rmail-undigest-test-rfc1153-less-strict-digest ()
  "Test that we can undigest a RFC 1153 with a Subject header in its footer."
  (let ((file (make-temp-file "undigest-test-")))
    (unwind-protect
        (with-temp-buffer
          (insert rmail-rfc1153-digest-less-strict)
          (write-region nil nil file)
          (rmail file)
          (undigestify-rmail-message)
          (should (= rmail-total-messages 5))
          (should (string= (rmail-message-content 3) "This is message one.\n\n"))
          (should (string= (rmail-message-content 4) "This is message two.\n\n")))
      (delete-file file))))

(ert-deftest rmail-undigest-test-rfc1153-sloppy-digest ()
  "Test that we can undigest a sloppy RFC 1153 digest."
  (let ((file (make-temp-file "undigest-test-")))
    (unwind-protect
        (with-temp-buffer
          (insert rmail-rfc1153-digest-sloppy)
          (write-region nil nil file)
          (rmail file)
          (undigestify-rmail-message)
          (should (= rmail-total-messages 5))
          (should (string= (rmail-message-content 3) "This is message one.\n\n"))
          (should (string= (rmail-message-content 4) "This is message two.\n\n")))
      (delete-file file))))

;; This fails because `rmail-digest-parse-mime' combines the preamble with the
;; first message of the digest.  And then, it doesn't get rid of the last
;; separator.
(ert-deftest rmail-undigest-test-rfc1521-mime-digest ()
  "Test that we can undigest a RFC 1521 MIME digest."
  :expected-result :failed
  (let ((file (make-temp-file "undigest-test-")))
    (unwind-protect
        (with-temp-buffer
          (insert rmail-rfc1521-mime-digest)
          (write-region nil nil file)
          (rmail file)
          (undigestify-rmail-message)
          (should (= rmail-total-messages 3))
          (should (string= (rmail-message-content 2) "Message one.\n\n"))
          (should (string= (rmail-message-content 3) "Message two.\n\n")))
      (delete-file file))))

(ert-deftest rmail-undigest-test-multipart-mixed-digest ()
  "Test that we can undigest a digest inside a multipart/mixed digest."
  (let ((file (make-temp-file "undigest-test-")))
    (unwind-protect
        (with-temp-buffer
          (insert rmail-multipart-mixed-digest)
          (write-region nil nil file)
          (rmail file)
          (undigestify-rmail-message)
          (should (= rmail-total-messages 4))
          (should (string= (rmail-message-content 2) "Message one.\n\n"))
          (should (string= (rmail-message-content 3) "Message two.\n\n")))
      (delete-file file))))
