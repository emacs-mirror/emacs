;;; coding-tests.el --- tests for text encoding and decoding

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>
;; Author: Kenichi Handa <handa@gnu.org>

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

;; Directory to hold test data files.
(defvar coding-tests-workdir
  (expand-file-name "coding-tests" temporary-file-directory))

;; Remove all generated test files.
(defun coding-tests-remove-files ()
  (delete-directory coding-tests-workdir t))

(ert-deftest ert-test-coding-bogus-coding-systems ()
  (unwind-protect
      (let (test-file)
        (or (file-directory-p coding-tests-workdir)
            (mkdir coding-tests-workdir t))
        (setq test-file (expand-file-name "nonexistent" coding-tests-workdir))
        (if (file-exists-p test-file)
            (delete-file test-file))
        (should-error
         (let ((coding-system-for-read 'bogus))
           (insert-file-contents test-file)))
        ;; See bug #21602.
        (setq test-file (expand-file-name "writing" coding-tests-workdir))
        (should-error
         (let ((coding-system-for-write (intern "\"us-ascii\"")))
           (write-region "some text" nil test-file))))
    (coding-tests-remove-files)))

;; See issue #5251.
(ert-deftest ert-test-unibyte-buffer-dos-eol-decode ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (encode-coding-string "あ" 'euc-jp) "\xd" "\n")
    (decode-coding-region (point-min) (point-max) 'euc-jp-dos)
    (should-not (string-match-p "\^M" (buffer-string)))))

;; Return the contents (specified by CONTENT-TYPE; ascii, latin, or
;; binary) of a test file.
(defun coding-tests-file-contents (content-type)
  (let* ((ascii "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n")
	 (latin (concat ascii "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ\n"))
	 (binary (string-to-multibyte
		  (concat (string-as-unibyte latin)
			  (unibyte-string #xC0 #xC1 ?\n)))))
    (cond ((eq content-type 'ascii) ascii)
	  ((eq content-type 'latin) latin)
	  ((eq content-type 'binary) binary)
	  (t
	   (error "Invalid file content type: %s" content-type)))))

;; Generate FILE with CONTENTS encoded by CODING-SYSTEM.
;; whose encoding specified by CODING-SYSTEM.
(defun coding-tests-gen-file (file contents coding-system)
  (or (file-directory-p coding-tests-workdir)
      (mkdir coding-tests-workdir t))
  (setq file (expand-file-name file coding-tests-workdir))
  (with-temp-file file
    (set-buffer-file-coding-system coding-system)
    (insert contents))
  file)

;;; The following three functions are filters for contents of a test
;;; file.

;; Convert all LFs to CR LF sequences in the string STR.
(defun coding-tests-lf-to-crlf (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (delete-char -1)
      (insert "\r\n"))
    (buffer-string)))

;; Convert all LFs to CRs in the string STR.
(defun coding-tests-lf-to-cr (str)
  (with-temp-buffer
    (insert str)
    (subst-char-in-region (point-min) (point-max) ?\n ?\r)
    (buffer-string)))

;; Convert all LFs to LF LF sequences in the string STR.
(defun coding-tests-lf-to-lflf (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (insert "\n"))
    (buffer-string)))

;; Prepend the UTF-8 BOM to STR.
(defun coding-tests-add-bom (str)
  (concat "\xfeff" str))

;; Return the name of test file whose contents specified by
;; CONTENT-TYPE and whose encoding specified by CODING-SYSTEM.
(defun coding-tests-filename (content-type coding-system &optional ext)
  (if ext
      (expand-file-name (format "%s-%s.%s" content-type coding-system ext)
			coding-tests-workdir)
    (expand-file-name (format "%s-%s" content-type coding-system)
		      coding-tests-workdir)))


;;; Check ASCII optimizing decoder

;; Generate a test file whose contents specified by CONTENT-TYPE and
;; whose encoding specified by CODING-SYSTEM.
(defun coding-tests-ao-gen-file (content-type coding-system)
  (let ((file (coding-tests-filename content-type coding-system)))
    (coding-tests-gen-file file
			    (coding-tests-file-contents content-type)
			    coding-system)))

;; Test the decoding of a file whose contents and encoding are
;; specified by CONTENT-TYPE and WRITE-CODING.  The test passes if the
;; file is read by READ-CODING and detected as DETECTED-CODING and the
;; contents is correctly decoded.
;; Optional 5th arg TRANSLATOR is a function to translate the original
;; file contents to match with the expected result of decoding.  For
;; instance, when a file of dos eol-type is read by unix eol-type,
;; `decode-test-lf-to-crlf' must be specified.

(defun coding-tests (content-type write-coding read-coding detected-coding
				   &optional translator)
  (prefer-coding-system 'utf-8-auto)
  (let ((filename (coding-tests-filename content-type write-coding)))
    (with-temp-buffer
      (let ((coding-system-for-read read-coding)
	    (contents (coding-tests-file-contents content-type))
	    (disable-ascii-optimization nil))
	(if translator
	    (setq contents (funcall translator contents)))
	(insert-file-contents filename)
	(if (and (coding-system-equal buffer-file-coding-system detected-coding)
		 (string= (buffer-string) contents))
	    nil
	  (list buffer-file-coding-system
		(string-to-list (buffer-string))
		(string-to-list contents)))))))

(ert-deftest ert-test-coding-ascii ()
  (unwind-protect
      (progn
	(dolist (eol-type '(unix dos mac))
	  (coding-tests-ao-gen-file 'ascii eol-type))
	(should-not (coding-tests 'ascii 'unix 'undecided 'unix))
	(should-not (coding-tests 'ascii 'dos 'undecided 'dos))
	(should-not (coding-tests 'ascii 'dos 'dos 'dos))
	(should-not (coding-tests 'ascii 'mac 'undecided 'mac))
	(should-not (coding-tests 'ascii 'mac 'mac 'mac))
	(should-not (coding-tests 'ascii 'dos 'utf-8 'utf-8-dos))
	(should-not (coding-tests 'ascii 'dos 'unix 'unix
				   'coding-tests-lf-to-crlf))
	(should-not (coding-tests 'ascii 'mac 'dos 'dos
				   'coding-tests-lf-to-cr))
	(should-not (coding-tests 'ascii 'dos 'mac 'mac
				   'coding-tests-lf-to-lflf)))
    (coding-tests-remove-files)))

(ert-deftest ert-test-coding-latin ()
  (unwind-protect
      (progn
	(dolist (coding '("utf-8" "utf-8-with-signature"))
	  (dolist (eol-type '("unix" "dos" "mac"))
	    (coding-tests-ao-gen-file 'latin
				       (intern (concat coding "-" eol-type)))))
	(should-not (coding-tests 'latin 'utf-8-unix 'undecided 'utf-8-unix))
	(should-not (coding-tests 'latin 'utf-8-unix 'utf-8-unix 'utf-8-unix))
	(should-not (coding-tests 'latin 'utf-8-dos 'undecided 'utf-8-dos))
	(should-not (coding-tests 'latin 'utf-8-dos 'utf-8-dos 'utf-8-dos))
	(should-not (coding-tests 'latin 'utf-8-mac 'undecided 'utf-8-mac))
	(should-not (coding-tests 'latin 'utf-8-mac 'utf-8-mac 'utf-8-mac))
	(should-not (coding-tests 'latin 'utf-8-dos 'unix 'utf-8-unix
				   'coding-tests-lf-to-crlf))
	(should-not (coding-tests 'latin 'utf-8-mac 'dos 'utf-8-dos
				   'coding-tests-lf-to-cr))
	(should-not (coding-tests 'latin 'utf-8-dos 'mac 'utf-8-mac
				   'coding-tests-lf-to-lflf))
	(should-not (coding-tests 'latin 'utf-8-with-signature-unix 'undecided
				   'utf-8-with-signature-unix))
	(should-not (coding-tests 'latin 'utf-8-with-signature-unix 'utf-8-auto
				   'utf-8-with-signature-unix))
	(should-not (coding-tests 'latin 'utf-8-with-signature-dos 'undecided
				   'utf-8-with-signature-dos))
	(should-not (coding-tests 'latin 'utf-8-with-signature-unix 'utf-8
				   'utf-8-unix 'coding-tests-add-bom))
	(should-not (coding-tests 'latin 'utf-8-with-signature-unix 'utf-8
				   'utf-8-unix 'coding-tests-add-bom)))
    (coding-tests-remove-files)))

(ert-deftest ert-test-coding-binary ()
  (unwind-protect
      (progn
	(dolist (eol-type '("unix" "dos" "mac"))
	  (coding-tests-ao-gen-file 'binary
				     (intern (concat "raw-text" "-" eol-type))))
	(should-not (coding-tests 'binary 'raw-text-unix 'undecided
				   'raw-text-unix))
	(should-not (coding-tests 'binary 'raw-text-dos 'undecided
				   'raw-text-dos))
	(should-not (coding-tests 'binary 'raw-text-mac 'undecided
				   'raw-text-mac))
	(should-not (coding-tests 'binary 'raw-text-dos 'unix
				   'raw-text-unix 'coding-tests-lf-to-crlf))
	(should-not (coding-tests 'binary 'raw-text-mac 'dos
				   'raw-text-dos 'coding-tests-lf-to-cr))
	(should-not (coding-tests 'binary 'raw-text-dos 'mac
				   'raw-text-mac 'coding-tests-lf-to-lflf)))
    (coding-tests-remove-files)))


;;; Check the coding system `prefer-utf-8'.

;; Read FILE.  Check if the encoding was detected as DETECT.  If
;; PREFER is non-nil, prefer that coding system before reading.

(defun coding-tests-prefer-utf-8-read (file detect prefer)
  (with-temp-buffer
    (with-coding-priority (if prefer (list prefer))
      (insert-file-contents file))
    (if (eq buffer-file-coding-system detect)
	nil
      (format "Invalid detection: %s" buffer-file-coding-system))))

;; Read FILE, modify it, and write it.  Check if the coding system
;; used for writing was CODING.  If CODING-TAG is non-nil, insert
;; coding tag with it before writing.  If STR is non-nil, insert it
;; before writing.

(defun coding-tests-prefer-utf-8-write (file coding-tag coding
					      &optional str)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if coding-tag
	(insert (format ";; -*- coding: %s; -*-\n" coding-tag))
      (insert ";;\n"))
    (if str
	(insert str))
    (write-file (coding-tests-filename 'test 'test "el"))
    (if (coding-system-equal buffer-file-coding-system coding)
	nil
      (format "Incorrect encoding: %s" last-coding-system-used))))

(ert-deftest ert-test-coding-prefer-utf-8 ()
  (unwind-protect
      (let ((ascii (coding-tests-gen-file "ascii.el"
					   (coding-tests-file-contents 'ascii)
					   'unix))
	    (latin (coding-tests-gen-file "utf-8.el"
					   (coding-tests-file-contents 'latin)
					   'utf-8-unix)))
	(should-not (coding-tests-prefer-utf-8-read
		     ascii 'prefer-utf-8-unix nil))
	(should-not (coding-tests-prefer-utf-8-read
		     latin 'utf-8-unix nil))
	(should-not (coding-tests-prefer-utf-8-read
		     latin 'utf-8-unix 'iso-8859-1))
	(should-not (coding-tests-prefer-utf-8-read
		     latin 'utf-8-unix 'sjis))
	(should-not (coding-tests-prefer-utf-8-write
		     ascii nil 'prefer-utf-8-unix))
	(should-not (coding-tests-prefer-utf-8-write
		     ascii 'iso-8859-1 'iso-8859-1-unix))
	(should-not (coding-tests-prefer-utf-8-write
		     ascii nil 'utf-8-unix "À")))
    (coding-tests-remove-files)))


;;; The following is for benchmark testing of the new optimized
;;; decoder, not for regression testing.

(defun generate-ascii-file ()
  (dotimes (i 100000)
    (insert-char ?a 80)
    (insert "\n")))

(defun generate-rarely-nonascii-file ()
  (dotimes (i 100000)
    (if (/= i 50000)
	(insert-char ?a 80)
      (insert ?À)
      (insert-char ?a 79))
    (insert "\n")))

(defun generate-mostly-nonascii-file ()
  (dotimes (i 30000)
    (insert-char ?a 80)
    (insert "\n"))
  (dotimes (i 20000)
    (insert-char ?À 80)
    (insert "\n"))
  (dotimes (i 10000)
    (insert-char ?あ 80)
    (insert "\n")))


(defvar test-file-list
  '((generate-ascii-file
     ("~/ascii-tag-utf-8-unix.unix" ";; -*- coding: utf-8-unix; -*-" unix)
     ("~/ascii-tag-utf-8.unix" ";; -*- coding: utf-8; -*-" unix)
     ("~/ascii-tag-none.unix" "" unix)
     ("~/ascii-tag-utf-8-dos.dos" ";; -*- coding: utf-8-dos; -*-" dos)
     ("~/ascii-tag-utf-8.dos" ";; -*- coding: utf-8; -*-" dos)
     ("~/ascii-tag-none.dos" "" dos))
    (generate-rarely-nonascii-file
     ("~/utf-8-r-tag-utf-8-unix.unix" ";; -*- coding: utf-8-unix; -*-" utf-8-unix)
     ("~/utf-8-r-tag-utf-8.unix" ";; -*- coding: utf-8; -*-" utf-8-unix)
     ("~/utf-8-r-tag-none.unix" "" utf-8-unix)
     ("~/utf-8-r-tag-utf-8-dos.dos" ";; -*- coding: utf-8-dos; -*-" utf-8-dos)
     ("~/utf-8-r-tag-utf-8.dos" ";; -*- coding: utf-8; -*-" utf-8-dos)
     ("~/utf-8-r-tag-none.dos" "" utf-8-dos))
    (generate-mostly-nonascii-file
     ("~/utf-8-m-tag-utf-8-unix.unix" ";; -*- coding: utf-8-unix; -*-" utf-8-unix)
     ("~/utf-8-m-tag-utf-8.unix" ";; -*- coding: utf-8; -*-" utf-8-unix)
     ("~/utf-8-m-tag-none.unix" "" utf-8-unix)
     ("~/utf-8-m-tag-utf-8-dos.dos" ";; -*- coding: utf-8-dos; -*-" utf-8-dos)
     ("~/utf-8-m-tag-utf-8.dos" ";; -*- coding: utf-8; -*-" utf-8-dos)
     ("~/utf-8-m-tag-none.dos" "" utf-8-dos))))

(defun generate-benchmark-test-file ()
  (interactive)
  (with-temp-buffer
    (message "Generating data...")
    (dolist (files test-file-list)
      (delete-region (point-min) (point-max))
      (funcall (car files))
      (dolist (file (cdr files))
	(message "Writing %s..." (car file))
	(goto-char (point-min))
	(insert (nth 1 file) "\n")
	(let ((coding-system-for-write (nth 2 file)))
	  (write-region (point-min) (point-max) (car file)))
	(delete-region (point-min) (point))))))

(defun benchmark-decoder ()
  (let ((gc-cons-threshold 4000000))
    (insert "Without optimization:\n")
    (dolist (files test-file-list)
      (dolist (file (cdr files))
	(let* ((disable-ascii-optimization t)
	       (result (benchmark-run 10
			 (with-temp-buffer (insert-file-contents (car file))))))
	  (insert (format "%s: %s\n"  (car file) result)))))
    (insert "With optimization:\n")
    (dolist (files test-file-list)
      (dolist (file (cdr files))
	(let* ((disable-ascii-optimization nil)
	       (result (benchmark-run 10
			 (with-temp-buffer (insert-file-contents (car file))))))
	  (insert (format "%s: %s\n" (car file) result)))))))

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:

(provide 'coding-tests)
;; coding-tests.el ends here
