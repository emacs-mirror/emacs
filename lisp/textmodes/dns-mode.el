;;; dns-mode.el --- a mode for viewing/editing Domain Name System master files  -*- lexical-binding: t -*-

;; Copyright (C) 2000-2001, 2004-2022 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: DNS master zone file SOA comm

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

;; Use M-x dns-mode RET to invoke in master files.
;;
;; C-c C-s  Increment SOA serial.
;;          Understands YYYYMMDDNN, Unix time, and serial number formats,
;;          and complains if it fail to find SOA serial.

;;; References:

;; RFC 1034, "DOMAIN NAMES - CONCEPTS AND FACILITIES", P. Mockapetris.
;; RFC 1035, "DOMAIN NAMES - IMPLEMENTATION AND SPECIFICATION", P. Mockapetris.
;; RFC 5155, "DNS Security (DNSSEC) Hashed Authenticated Denial of Existence"
;; RFC 6698, "The DNS-Based Authentication of Named Entities (DANE)
;;             Transport Layer Security (TLS) Protocol: TLSA"
;; RFC 6844, "DNS Certification Authority Authorization (CAA) Resource Record"

;;; Release history:

;; 2004-09-11  Posted on gnu.emacs.sources.
;; 2004-09-13  Ported to XEmacs.
;; 2004-09-14  Installed in Emacs CVS.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup dns-mode nil
  "DNS master file mode configuration."
  :group 'data)

(defconst dns-mode-control-entities '("INCLUDE" "ORIGIN" "TTL")
  "Lists of strings with known DNS control entities.")

(defconst dns-mode-classes '("IN" "CS" "CH" "HS")
  "List of strings with known DNS classes.")

(defconst dns-mode-types '("A" "NS" "MD" "MF" "CNAME" "SOA" "MB" "MG" "MR"
			   "NULL" "WKS" "PTR" "HINFO" "MINFO" "MX" "TXT"
			   "RP" "AFSDB" "X25" "ISDN" "RT" "NSAP"
			   "SIG" "KEY" "PX" "GPOS" "AAAA" "LOC" "NXT"
			   "EID" "NIMLOC" "SRV" "ATMA" "NAPTR" "KX" "CERT"
			   "A6" "DNAME" "SINK" "OPT" "APL" "DS" "SSHFP"
			   "RRSIG" "NSEC" "DNSKEY" "UINFO" "UID" "GID"
			   "UNSPEC" "TKEY" "TSIG" "IXFR" "AXFR" "MAILB"
			   "MAILA" "TLSA" "NSEC3" "CAA")
  "List of strings with known DNS types.")

(defface dns-mode-control-entity '((t :inherit font-lock-keyword-face))
  "Face used for DNS control entities, e.g. $ORIGIN."
  :version "26.1")

(defface dns-mode-bad-control-entity '((t :inherit font-lock-warning-face))
  "Face used for non-standard DNS control entities, e.g. $FOO."
  :version "26.1")

(defface dns-mode-type '((t :inherit font-lock-type-face))
  "Face used for DNS types, e.g., SOA."
  :version "26.1")

(defface dns-mode-class '((t :inherit font-lock-constant-face))
  "Face used for DNS classes, e.g., IN."
  :version "26.1")

(defvar dns-mode-control-entity-face ''dns-mode-control-entity
  "Name of face used for control entities, e.g. $ORIGIN.")
(make-obsolete-variable 'dns-mode-control-entity-face
			"customize the face `dns-mode-control-entity' instead."
			"26.1" 'set)

(defvar dns-mode-bad-control-entity-face ''dns-mode-bad-control-entity
  "Name of face used for non-standard control entities, e.g. $FOO.")
(make-obsolete-variable
 'dns-mode-bad-control-entity-face
 "customize the face `dns-mode-bad-control-entity' instead."
 "26.1" 'set)

(defvar dns-mode-type-face ''dns-mode-type
  "Name of face used for DNS types, e.g., SOA.")
(make-obsolete-variable 'dns-mode-type-face
			"customize the face `dns-mode-type' instead."
			"26.1" 'set)

(defvar dns-mode-class-face ''dns-mode-class
  "Name of face used for DNS classes, e.g., IN.")
(make-obsolete-variable 'dns-mode-class
			"customize the face `dns-mode-class' instead."
			"26.1" 'set)

(defcustom dns-mode-font-lock-keywords
  `((,(concat "^\\$" (regexp-opt dns-mode-control-entities))
     0 ,dns-mode-control-entity-face)
    ("^\\$[a-z0-9A-Z]+" 0 ,dns-mode-bad-control-entity-face)
    (,(regexp-opt dns-mode-classes) 0 ,dns-mode-class-face)
    (,(regexp-opt dns-mode-types) 0 ,dns-mode-type-face))
  "Font lock keywords used to highlight text in DNS master file mode."
  :version "26.1"
  :type 'sexp)

(defcustom dns-mode-soa-auto-increment-serial t
  "Whether to increment the SOA serial number automatically.

If this variable is t, the serial number is incremented upon each save of
the file.  If it is `ask', Emacs asks for confirmation whether it should
increment the serial upon saving.  If nil, serials must be incremented
manually with \\[dns-mode-soa-increment-serial]."
  :type '(choice (const :tag "Always" t)
		 (const :tag "Ask" ask)
		 (const :tag "Never" nil))
  :safe 'symbolp)

;; Syntax table.

(defvar dns-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<   " table)
    (modify-syntax-entry ?\n ">   " table)
    table)
  "Syntax table in use in DNS master file buffers.")

;; Keymap.

(defvar dns-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" #'dns-mode-soa-increment-serial)
    (define-key map "\C-c\C-e" #'dns-mode-ipv6-to-nibbles)
    map)
  "Keymap for DNS master file mode.")

;; Menu.

(defvar dns-mode-menu nil
  "Menubar used in DNS master file mode.")

(easy-menu-define dns-mode-menu dns-mode-map
  "DNS Menu."
  '("DNS"
    ["Increment SOA serial" dns-mode-soa-increment-serial t]
    ["Convert IPv6 address to nibbles" dns-mode-ipv6-to-nibbles t]))

;; Mode.

;;;###autoload
(define-derived-mode dns-mode text-mode "DNS"
  "Major mode for viewing and editing DNS master files.
This mode is inherited from text mode.  It add syntax
highlighting, and some commands for handling DNS master files.
Its keymap inherits from `text-mode' and it has the same
variables for customizing indentation.  It has its own abbrev
table and its own syntax table.

Turning on DNS mode runs `dns-mode-hook'."
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  (setq-local font-lock-defaults
              '(dns-mode-font-lock-keywords nil nil ((?_ . "w"))))
  (add-hook 'before-save-hook #'dns-mode-soa-maybe-increment-serial
            nil t))

;;;###autoload (defalias 'zone-mode 'dns-mode)

;; Tools.

;;;###autoload
(defun dns-mode-soa-increment-serial ()
  "Locate SOA record and increment the serial field."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward
	     (concat "^\\(\\(\\([^ \t]+[ \t]+\\)?[^ \t]+"
		     "[ \t]+\\)?[^ \t]+[ \t]+\\)?SOA") nil t)
      (error "Cannot locate SOA record"))
    (if (re-search-forward (concat "\\<\\("
				   ;; year
				   "\\(198\\|199\\|20[0-9]\\)[0-9]"
				   ;; month
				   "\\(0[0-9]\\|10\\|11\\|12\\)"
				   ;; day
				   "\\([012][0-9]\\|30\\|31\\)"
				   ;; counter
				   "\\([0-9]\\{1,3\\}\\)"
				   "\\)\\>")
			   nil t)
	;; YYYYMMDDIII format, one to three I's.
	(let* ((serial (match-string 1))
	       (counterstr (match-string 5))
	       (counter (string-to-number counterstr))
	       (now (format-time-string "%Y%m%d"))
	       (nowandoldserial (concat now counterstr)))
	  (if (string< serial nowandoldserial)
	      (let ((new (format "%s00" now)))
		(replace-match new nil nil nil 1)
		(message "Replaced old serial %s with %s" serial new))
	    (if (string= serial nowandoldserial)
		(let ((new (format (format "%%s%%0%dd" (length counterstr))
				   now (1+ counter))))
		  (replace-match new nil nil nil 1)
		  (message "Replaced old serial %s with %s" serial new))
	      (error "Current SOA serial is in the future"))))
      (if (re-search-forward "\\<\\([0-9]\\{9,10\\}\\)\\>" nil t)
	  ;; Unix time
	  (let* ((serial (match-string 1))
		 (new (format-time-string "%s")))
	    (if (not (string< serial new))
		(error "Current SOA serial is in the future")
	      (replace-match new nil nil nil 1)
	      (message "Replaced old serial %s with %s" serial new)))
	(if (re-search-forward "\\<\\([0-9]+\\)\\>" nil t)
	    ;; Just any serial number.
	    (let* ((serial (match-string 1))
		   (new (format "%d" (1+ (string-to-number serial)))))
	      (replace-match new nil nil nil 1)
	      (message "Replaced old serial %s with %s" serial new))
	  (error "Cannot locate serial number in SOA record"))))))

(defun dns-mode-soa-maybe-increment-serial ()
  "Increment SOA serial if needed.

This function is run from `before-save-hook'."
  (when (and (buffer-modified-p)
	     dns-mode-soa-auto-increment-serial
	     (or (eq dns-mode-soa-auto-increment-serial t)
		 (y-or-n-p "Increment SOA serial? ")))
    ;; If `dns-mode-soa-increment-serial' signals an error saving will
    ;; fail but that probably means that the serial should be fixed to
    ;; comply with the RFC anyway! -rfr
    (progn (dns-mode-soa-increment-serial)
	   ;; We return nil in case this is used in write-contents-functions.
	   nil)))

(defun dns-mode-ipv6-to-nibbles (&optional negate-prefix)
  "Convert an IPv6 address around or before point.
Replace the address by its ip6.arpa-representation for use in
reverse zone files, placing the original address in the kill ring.

The address can be: a complete address (no prefix designator);
with a normal prefix designator (e.g. /48), in which case only
the required number of nibbles are output; or with a negative
prefix designator (e.g. /-112), in which case only the part of
the address *not* covered by the absolute value of the prefix
length is output, as a relative address (without \".ip6.arpa.\" at
the end).  This is useful when $ORIGIN is specified in the zone file.

Optional prefix argument NEGATE-PREFIX negates the value of the
detected prefix length.

Examples:

2001:db8::12  =>
2.1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.ip6.arpa.

2001:db8::12/32  =>
8.b.d.0.1.0.0.2.ip6.arpa.

2001:db8::12/-32  =>
2.1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0

::42/112 (with prefix argument) =>
2.4.0.0"
  (interactive "P")
  (skip-syntax-backward " ")
  (skip-syntax-backward "w_.")
  (re-search-forward "\\([[:xdigit:]:]+\\)\\(/-?[0-9]\\{2,3\\}\\)?")
  (let ((address (match-string 1))
        (prefix-length (match-string 2)))
    (kill-new (match-string 0))
    (when prefix-length
      (setq prefix-length (string-to-number (substring prefix-length 1)))
      (if negate-prefix
          (setq prefix-length (- prefix-length))))
    (replace-match
     (save-match-data
       (dns-mode-reverse-and-expand-ipv6 address prefix-length)))))

(defun dns-mode-reverse-and-expand-ipv6 (address &optional prefix-length)
  "Convert an IPv6 address to (parts of) an ip6.arpa nibble format.
ADDRESS is an IPv6 address in the usual colon-separated
format, without a prefix designator at the end.

Optional PREFIX-LENGTH is a number whose absolute value is the
length in bits of the network part of the address.  If nil,
return an absolute address representing the full IPv6 address.
If positive, return an absolute address representing the network
prefix indicated.  If negative, return a relative address
representing the host parts of the address with respect to the
indicated network prefix.

See `dns-mode-ipv6-to-nibbles' for examples."
  (let* ((chunks (split-string address ":"))
         (prefix-length-nibbles (if prefix-length
                                    (ceiling (abs prefix-length) 4)
                                  32))
         (filler-chunks (- 8 (length (remove "" chunks))))
         (expanded-address
          (apply #'concat
                 (cl-loop with filler-done = nil
                          for chunk in chunks
                          if (and (not filler-done)
                                  (string= "" chunk))
                          append (prog1
                                     (cl-loop repeat filler-chunks
                                              collect "0000")
                                   (setq filler-done t))
                          else
                          if (not (string= "" chunk))
                          collect (format "%04x"
                                          (string-to-number chunk 16)))))
         (rev-address-nibbles
          (nreverse (if (and prefix-length
                             (cl-minusp prefix-length))
                        (substring expanded-address prefix-length-nibbles)
                      (substring expanded-address 0 prefix-length-nibbles)))))
    (with-temp-buffer
      (cl-loop for char across rev-address-nibbles
               do
               (insert char)
               (insert "."))
      (if (and prefix-length
               (cl-minusp prefix-length))
          (delete-char -1)
        (insert "ip6.arpa."))
      (insert " ")
      (buffer-string))))

(provide 'dns-mode)

;;; dns-mode.el ends here
