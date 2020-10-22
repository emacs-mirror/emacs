;;; exif.el --- parsing Exif data in JPEG images -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: images

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

;; Specification at:

;; https://www.media.mit.edu/pia/Research/deepview/exif.html
;; but it's kinda er not very easy to read.

;; The JPEG format is:
;;
;; FFD8 and then any number of chunks on the format: FFxx SSSS ...,
;; where FFxx is the ID, and SSSS is the length of the chunk plus 2.
;; When you get to ID FFDA, the image itself is over and you can stop
;; parsing.
;;
;; The Exif data is in the TIFF format.  It starts off with the six
;; bytes "Exif^0^0".
;;
;; Then either "II" or "MM", where "II" means little-endian and "MM"
;; means big-endian.  All subsequent numbers should be read in
;; according to this.
;;
;; Next follows two bytes that should always represent 0x2a, and then
;; four bytes that's the offset to where the IFD "image file
;; directory" starts.  (It's an offset from the start of this chunk;
;; i.e., where "II"/"MM" is; all offsets in the TIFF format are from
;; this point.)
;;
;; The IFD starts with two bytes that says how many entries there are
;; in the directory, and then that number of entries follows, and then
;; an offset to the next IFD.

;; Usage: (exif-parse-file "test.jpg") =>
;; ((:tag 274 :tag-name orientation :format 3 :format-type short :value 1)
;;  (:tag 282 :tag-name x-resolution :format 5 :format-type rational :value
;;        (180 . 1))
;;  (:tag 306 :tag-name date-time :format 2 :format-type ascii
;;   :value "2019:09:21 16:22:13")
;;   ...)

;;; Code:

(require 'cl-lib)

(defvar exif-tag-alist
  '((11 processing-software)
    (271 make)
    (272 model)
    (274 orientation)
    (282 x-resolution)
    (283 y-resolution)
    (296 resolution-unit)
    (305 software)
    (306 date-time)
    (315 artist))
  "Alist of tag values and their names.")

(defconst exif--orientation
  '((1 0 nil)
    (2 0 t)
    (3 180 nil)
    (4 180 t)
    (5 90 nil)
    (6 90 t)
    (7 270 nil)
    (8 270 t))
  "Alist of Exif orientation codes.
These are mapped onto rotation values and whether the image is
mirrored or not.")

(define-error 'exif-error "Invalid Exif data")

(defun exif-parse-file (file)
  "Parse FILE (a JPEG file) and return the Exif data, if any.
The return value is a list of Exif items.

If the data is invalid, an `exif-error' is signaled."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (exif-parse-buffer)))

(defun exif-parse-buffer (&optional buffer)
  "Parse BUFFER (which should be a JPEG file) and return the Exif data, if any.
The return value is a list of Exif items.

If the data is invalid, an `exif-error' is signaled."
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (if enable-multibyte-characters
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let ((dest (current-buffer)))
            (with-current-buffer buffer
              (encode-coding-region (point-min) (point-max)
                                    buffer-file-coding-system
                                    dest))
            (when-let ((app1 (cdr (assq #xffe1 (exif--parse-jpeg)))))
              (exif--parse-exif-chunk app1))))
      (when-let ((app1 (cdr (assq #xffe1 (exif--parse-jpeg)))))
        (exif--parse-exif-chunk app1)))))

(defun exif-orientation (exif)
  "Return the orientation (in degrees) in EXIF.
If the orientation isn't present in the data, return nil."
  (let ((code (plist-get (cl-find 'orientation exif
                                  :key (lambda (e)
                                         (plist-get e :tag-name)))
                         :value)))
    (cadr (assq code exif--orientation))))

(defun exif--parse-jpeg ()
  (unless (= (exif--read-number-be 2) #xffd8) ; SOI (start of image)
    (signal 'exif-error "Not a valid JPEG file"))
  (cl-loop for segment = (exif--read-number-be 2)
           for size = (exif--read-number-be 2)
           ;; Stop parsing when we get to SOS (start of stream);
           ;; this is when the image itself starts, and there will
           ;; be no more chunks of interest after that.
           while (not (= segment #xffda))
           collect (cons segment (exif--read-chunk (- size 2)))))

(defun exif--parse-exif-chunk (data)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert data)
    (goto-char (point-min))
    ;; The Exif data is in the APP1 JPEG chunk and starts with
    ;; "Exif\0\0".
    (unless (equal (exif--read-chunk 6) (string ?E ?x ?i ?f ?\0 ?\0))
      (signal 'exif-error "Not a valid Exif chunk"))
    (delete-region (point-min) (point))
    (let* ((endian-marker (exif--read-chunk 2))
           (le (cond
                ;; "Motorola" is big-endian.
                ((equal endian-marker "MM")
                 nil)
                ;; "Intel" is little-endian.
                ((equal endian-marker "II")
                 t)
                (t
                 (signal 'exif-error
                         (format "Invalid endian-ness %s" endian-marker))))))
      ;; Another magical number.
      (unless (= (exif--read-number 2 le) #x002a)
        (signal 'exif-error "Invalid TIFF header length"))
      (let ((offset (exif--read-number 2 le)))
        ;; Jump to where the IFD (directory) starts and parse it.
        (when (> (1+ offset) (point-max))
          (signal 'exif-error "Invalid IFD (directory) offset"))
        (goto-char (1+ offset))
        (exif--parse-directory le)))))

(defun exif--field-format (number)
  (cl-case number
    (1 (cons 'byte 1))
    (2 (cons 'ascii 1))
    (3 (cons 'short 2))
    (4 (cons 'long 4))
    (5 (cons 'rational 8))
    (otherwise (cons 'unknown 1))))

(defun exif--parse-directory (le)
  (let ((dir
         (cl-loop repeat (exif--read-number 2 le)
                  for tag = (exif--read-number 2 le)
                  for format = (exif--read-number 2 le)
                  for field-format = (exif--field-format format)
                  ;; The actual length is the number in this field
                  ;; times the "inherent" length of the field format
                  ;; (i.e., "long integer" (4 bytes) or "ascii" (1
                  ;; byte)).
                  for length = (* (exif--read-number 4 le)
                                  (cdr field-format))
                  for value = (exif--read-number 4 le)
                  collect (list
                           :tag tag
                           :tag-name (cadr (assq tag exif-tag-alist))
                           :format format
                           :format-type (car field-format)
                           :value (exif--process-value
                                   (if (> length 4)
                                       ;; If the length of the data is
                                       ;; more than 4 bytes, then it's
                                       ;; actually stored after this
                                       ;; directory, and the value
                                       ;; here is just the offset to
                                       ;; use to find the data.
                                       (progn
                                         (when (> (+ (1+ value) length)
                                                  (point-max))
                                           (signal 'exif-error
                                                   "Premature end of file"))
                                         (buffer-substring
                                          (1+ value)
                                          (+ (1+ value) length)))
                                     ;; The value is stored directly
                                     ;; in the directory.
                                     (if (eq (car field-format) 'ascii)
                                         (exif--direct-ascii-value
                                          value (1- length) le)
                                       value))
                                   (car field-format)
                                   le)))))
    (let ((next (exif--read-number 4 le)))
      (if (> next 0)
          ;; There's more than one directory; if so, jump to it and
          ;; keep parsing.
          (progn
            (when (> (1+ next) (point-max))
              (signal 'exif-error "Invalid IFD (directory) next-offset"))
            (goto-char (1+ next))
            (nconc dir (exif--parse-directory le)))
        ;; We've reached the end of the directories.
        dir))))

(defun exif--direct-ascii-value (value bytes le)
  "Make VALUE into a zero-terminated string.
VALUE is an integer representing BYTES characters."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if le
        (dotimes (i bytes)
          (insert (logand (lsh value (* i -8)) 255)))
      (dotimes (i bytes)
        (insert (logand (lsh value (* (- (1- bytes) i) -8)) 255))))
    (insert 0)
    (buffer-string)))

(defun exif--process-value (value type le)
  "Do type-based post-processing of the value."
  (cl-case type
    ;; Chop off trailing zero byte.
    ('ascii (substring value 0 (1- (length value))))
    ('rational (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert value)
                 (goto-char (point-min))
                 (cons (exif--read-number 4 le)
                       (exif--read-number 4 le))))
    (otherwise value)))

(defun exif--read-chunk (bytes)
  "Return BYTES octets from the buffer and advance point that much."
  (when (> (+ (point) bytes) (point-max))
    (signal 'exif-error "Premature end of file"))
  (prog1
      (buffer-substring (point) (+ (point) bytes))
    (forward-char bytes)))

(defun exif--read-number-be (bytes)
  "Read BYTES octets from the buffer as a chunk of big-endian bytes.
Advance point to after the read bytes."
  (when (> (+ (point) bytes) (point-max))
    (signal 'exif-error "Premature end of file"))
  (let ((sum 0))
    (dotimes (_ bytes)
      (setq sum (+ (* sum 256) (following-char)))
      (forward-char 1))
    sum))

(defun exif--read-number-le (bytes)
  "Read BYTES octets from the buffer as a chunk of low-endian bytes.
Advance point to after the read bytes."
  (when (> (+ (point) bytes) (point-max))
    (signal 'exif-error "Premature end of file"))
  (let ((sum 0))
    (dotimes (i bytes)
      (setq sum (+ (* (following-char) (expt 256 i)) sum))
      (forward-char 1))
    sum))

(defun exif--read-number (bytes lower-endian)
  "Read BYTES octets from the buffer with endianness determined by LOWER-ENDIAN.
Advance point to after the read bytes."
  (if lower-endian
      (exif--read-number-le bytes)
    (exif--read-number-be bytes)))

(provide 'exif)

;;; exif.el ends here
