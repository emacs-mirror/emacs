;;; rmailmm.el --- tests for mail/rmailmm.el

;; Copyright (C) 2006-2020 Free Software Foundation, Inc.

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

(require 'rmailmm)

(defun rmailmm-test-handler ()
  "Test of a mail using no MIME parts at all."
  (let ((mail "To: alex@gnu.org
Content-Type: text/plain; charset=koi8-r
Content-Transfer-Encoding: 8bit
MIME-Version: 1.0

\372\304\322\301\327\323\324\327\325\312\324\305\41"))
    (switch-to-buffer (get-buffer-create "*test*"))
    (erase-buffer)
    (set-buffer-multibyte nil)
    (insert mail)
    (rmail-mime-show t)
    (set-buffer-multibyte t)))

(defun rmailmm-test-bulk-handler ()
  "Test of a mail used as an example in RFC 2183."
  (let ((mail "Content-Type: image/jpeg
Content-Disposition: attachment; filename=genome.jpeg;
  modification-date=\"Wed, 12 Feb 1997 16:29:51 -0500\";
Content-Description: a complete map of the human genome
Content-Transfer-Encoding: base64

iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAABGdBTUEAALGPC/xhBQAAAAZQ
TFRF////AAAAVcLTfgAAAPZJREFUeNq9ldsOwzAIQ+3//+l1WlvA5ZLsoUiTto4TB+ISoAjy
+ITfRBfcAmgRFFeAm+J6uhdKdFhFWUgDkFsK0oUp/9G2//Kj7Jx+5tSKOdBscgUYiKHRS/me
WATQdRUvAK0Bnmshmtn79PpaLBbbOZkjKvRnjRZoRswOkG1wFchKew2g9wXVJVZL/m4+B+vv
9AxQQR2Q33SgAYJzzVACdAWjAfRYzYFO9n6SLnydtQHSMxYDMAKqZ/8FS/lTK+zuq3CtK64L
UDwbgUEAUmk2Zyg101d6PhCDySgAvTvDgKiuOrc4dLxUb7UMnhGIexyI+d6U+ABuNAP4Simx
lgAAAABJRU5ErkJggg==
"))
    (switch-to-buffer (get-buffer-create "*test*"))
    (erase-buffer)
    (insert mail)
    (rmail-mime-show)))

(defun rmailmm-test-multipart-handler ()
  "Test of a mail used as an example in RFC 2046."
  (let ((mail "From: Nathaniel Borenstein <nsb@bellcore.com>
To: Ned Freed <ned@innosoft.com>
Date: Sun, 21 Mar 1993 23:56:48 -0800 (PST)
Subject: Sample message
MIME-Version: 1.0
Content-type: multipart/mixed; boundary=\"simple boundary\"

This is the preamble.  It is to be ignored, though it
is a handy place for composition agents to include an
explanatory note to non-MIME conformant readers.

--simple boundary

This is implicitly typed plain US-ASCII text.
It does NOT end with a linebreak.
--simple boundary
Content-type: text/plain; charset=us-ascii

This is explicitly typed plain US-ASCII text.
It DOES end with a linebreak.

--simple boundary--

This is the epilogue.  It is also to be ignored."))
    (switch-to-buffer (get-buffer-create "*test*"))
    (erase-buffer)
    (insert mail)
    (rmail-mime-show t)))

;;; rmailmm.el ends here
