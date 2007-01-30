;;; dns.el --- Domain Name Service lookups

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: network

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'mm-util)

(defvar dns-timeout 5
  "How many seconds to wait when doing DNS queries.")

(defvar dns-servers nil
  "Which DNS servers to query.
If nil, /etc/resolv.conf will be consulted.")

;;; Internal code:

(defvar dns-query-types
  '((A 1)
    (NS 2)
    (MD 3)
    (MF 4)
    (CNAME 5)
    (SOA 6)
    (MB 7)
    (MG 8)
    (MR 9)
    (NULL 10)
    (WKS 11)
    (PRT 12)
    (HINFO 13)
    (MINFO 14)
    (MX 15)
    (TXT 16)
    (AXFR 252)
    (MAILB 253)
    (MAILA 254)
    (* 255))
  "Names of query types and their values.")

(defvar dns-classes
  '((IN 1)
    (CS 2)
    (CH 3)
    (HS 4))
  "Classes of queries.")

(defun dns-write-bytes (value &optional length)
  (let (bytes)
    (dotimes (i (or length 1))
      (push (% value 256) bytes)
      (setq value (/ value 256)))
    (dolist (byte bytes)
      (insert byte))))

(defun dns-read-bytes (length)
  (let ((value 0))
    (dotimes (i length)
      (setq value (logior (* value 256) (following-char)))
      (forward-char 1))
    value))

(defun dns-get (type spec)
  (cadr (assq type spec)))

(defun dns-inverse-get (value spec)
  (let ((found nil))
    (while (and (not found)
		spec)
      (if (eq value (cadr (car spec)))
	  (setq found (caar spec))
	(pop spec)))
    found))

(defun dns-write-name (name)
  (dolist (part (split-string name "\\."))
    (dns-write-bytes (length part))
    (insert part))
  (dns-write-bytes 0))

(defun dns-read-string-name (string buffer)
  (mm-with-unibyte-buffer
    (insert string)
    (goto-char (point-min))
    (dns-read-name buffer)))

(defun dns-read-name (&optional buffer)
  (let ((ended nil)
	(name nil)
	length)
    (while (not ended)
      (setq length (dns-read-bytes 1))
      (if (= 192 (logand length (lsh 3 6)))
	  (let ((offset (+ (* (logand 63 length) 256)
			   (dns-read-bytes 1))))
	    (save-excursion
	      (when buffer
		(set-buffer buffer))
	      (goto-char (1+ offset))
	      (setq ended (dns-read-name buffer))))
	(if (zerop length)
	    (setq ended t)
	  (push (buffer-substring (point)
				  (progn (forward-char length) (point)))
		name))))
    (if (stringp ended)
	(if (null name)
	    ended
	  (concat (mapconcat 'identity (nreverse name) ".") "." ended))
      (mapconcat 'identity (nreverse name) "."))))

(defun dns-write (spec &optional tcp-p)
  "Write a DNS packet according to SPEC.
If TCP-P, the first two bytes of the package with be the length field."
  (with-temp-buffer
    (dns-write-bytes (dns-get 'id spec) 2)
    (dns-write-bytes
     (logior
      (lsh (if (dns-get 'response-p spec) 1 0) -7)
      (lsh
       (cond
	((eq (dns-get 'opcode spec) 'query) 0)
	((eq (dns-get 'opcode spec) 'inverse-query) 1)
	((eq (dns-get 'opcode spec) 'status) 2)
	(t (error "No such opcode: %s" (dns-get 'opcode spec))))
       -3)
      (lsh (if (dns-get 'authoritative-p spec) 1 0) -2)
      (lsh (if (dns-get 'truncated-p spec) 1 0) -1)
      (lsh (if (dns-get 'recursion-desired-p spec) 1 0) 0)))
    (dns-write-bytes
     (cond 
      ((eq (dns-get 'response-code spec) 'no-error) 0)
      ((eq (dns-get 'response-code spec) 'format-error) 1)
      ((eq (dns-get 'response-code spec) 'server-failure) 2)
      ((eq (dns-get 'response-code spec) 'name-error) 3)
      ((eq (dns-get 'response-code spec) 'not-implemented) 4)
      ((eq (dns-get 'response-code spec) 'refused) 5)
      (t 0)))
    (dns-write-bytes (length (dns-get 'queries spec)) 2)
    (dns-write-bytes (length (dns-get 'answers spec)) 2)
    (dns-write-bytes (length (dns-get 'authorities spec)) 2)
    (dns-write-bytes (length (dns-get 'additionals spec)) 2)
    (dolist (query (dns-get 'queries spec))
      (dns-write-name (car query))
      (dns-write-bytes (cadr (assq (or (dns-get 'type query) 'A)
				   dns-query-types)) 2)
      (dns-write-bytes (cadr (assq (or (dns-get 'class query) 'IN)
				   dns-classes)) 2))
    (dolist (slot '(answers authorities additionals))
      (dolist (resource (dns-get slot spec))
	(dns-write-name (car resource))
      (dns-write-bytes (cadr (assq (dns-get 'type resource) dns-query-types))
		       2)
      (dns-write-bytes (cadr (assq (dns-get 'class resource) dns-classes))
		       2)
      (dns-write-bytes (dns-get 'ttl resource) 4)
      (dns-write-bytes (length (dns-get 'data resource)) 2)
      (insert (dns-get 'data resource))))
    (when tcp-p
      (goto-char (point-min))
      (dns-write-bytes (buffer-size) 2))
    (buffer-string)))

(defun dns-read (packet)
  (mm-with-unibyte-buffer
    (let ((spec nil)
	  queries answers authorities additionals)
      (insert packet)
      (goto-char (point-min))
      (push (list 'id (dns-read-bytes 2)) spec)
      (let ((byte (dns-read-bytes 1)))
	(push (list 'response-p (if (zerop (logand byte (lsh 1 7))) nil t))
	      spec)
	(let ((opcode (logand byte (lsh 7 3))))
	  (push (list 'opcode
		      (cond ((eq opcode 0) 'query)
			    ((eq opcode 1) 'inverse-query)
			    ((eq opcode 2) 'status)))
		spec))
	(push (list 'authoritative-p (if (zerop (logand byte (lsh 1 2)))
					 nil t)) spec)
	(push (list 'truncated-p (if (zerop (logand byte (lsh 1 2))) nil t))
	      spec)
	(push (list 'recursion-desired-p
		    (if (zerop (logand byte (lsh 1 0))) nil t)) spec))
      (let ((rc (logand (dns-read-bytes 1) 15)))
	(push (list 'response-code
		    (cond
		     ((eq rc 0) 'no-error)
		     ((eq rc 1) 'format-error)
		     ((eq rc 2) 'server-failure)
		     ((eq rc 3) 'name-error)
		     ((eq rc 4) 'not-implemented)
		     ((eq rc 5) 'refused)))
	      spec))
      (setq queries (dns-read-bytes 2))
      (setq answers (dns-read-bytes 2))
      (setq authorities (dns-read-bytes 2))
      (setq additionals (dns-read-bytes 2))
      (let ((qs nil))
	(dotimes (i queries)
	  (push (list (dns-read-name)
		      (list 'type (dns-inverse-get (dns-read-bytes 2)
						   dns-query-types))
		      (list 'class (dns-inverse-get (dns-read-bytes 2)
						    dns-classes)))
		qs))
	(push (list 'queries qs) spec))
    (dolist (slot '(answers authorities additionals))
      (let ((qs nil)
	    type)
	(dotimes (i (symbol-value slot))
	  (push (list (dns-read-name)
		      (list 'type
			    (setq type (dns-inverse-get (dns-read-bytes 2)
							dns-query-types)))
		      (list 'class (dns-inverse-get (dns-read-bytes 2)
						    dns-classes))
		      (list 'ttl (dns-read-bytes 4))
		      (let ((length (dns-read-bytes 2)))
			(list 'data
			      (dns-read-type
			       (buffer-substring
				(point)
				(progn (forward-char length) (point)))
			       type))))
		qs))
	(push (list slot qs) spec)))
    (nreverse spec))))

(defun dns-read-type (string type)
  (let ((buffer (current-buffer))
	(point (point)))
    (prog1
	(mm-with-unibyte-buffer
	  (insert string)
	  (goto-char (point-min))
	  (cond
	   ((eq type 'A)
	    (let ((bytes nil))
	      (dotimes (i 4)
		(push (dns-read-bytes 1) bytes))
	      (mapconcat 'number-to-string (nreverse bytes) ".")))
	   ((eq type 'NS)
	    (dns-read-string-name string buffer))
	   ((eq type 'CNAME)
	    (dns-read-string-name string buffer))
	   (t string)))
      (goto-char point))))

(defun dns-parse-resolv-conf ()
  (when (file-exists-p "/etc/resolv.conf")
    (with-temp-buffer
      (insert-file-contents "/etc/resolv.conf")
      (goto-char (point-min))
      (while (re-search-forward "^nameserver[\t ]+\\([^ \t\n]+\\)" nil t)
	(push (match-string 1) dns-servers))
      (setq dns-servers (nreverse dns-servers)))))

;;; Interface functions.
(eval-when-compile
  (when (featurep 'xemacs)
    (require 'gnus-xmas)))

(defmacro dns-make-network-process (server)
  (if (featurep 'xemacs)
      `(let ((coding-system-for-read 'binary)
	     (coding-system-for-write 'binary))
	 (gnus-xmas-open-network-stream "dns" (current-buffer)
					,server "domain" 'udp))
    `(let ((server ,server)
	   (coding-system-for-read 'binary)
	   (coding-system-for-write 'binary))
       (if (fboundp 'make-network-process)
	   (make-network-process
	    :name "dns"
	    :coding 'binary
	    :buffer (current-buffer)
	    :host server
	    :service "domain"
	    :type 'datagram)
	 ;; Older versions of Emacs doesn't have
	 ;; `make-network-process', so we fall back on opening a TCP
	 ;; connection to the DNS server.
	 (open-network-stream "dns" (current-buffer) server "domain")))))

(defun query-dns (name &optional type fullp)
  "Query a DNS server for NAME of TYPE.
If FULLP, return the entire record returned."
  (setq type (or type 'A))
  (unless dns-servers
    (dns-parse-resolv-conf))

  (if (not dns-servers)
      (message "No DNS server configuration found")
    (mm-with-unibyte-buffer
      (let ((process (condition-case ()
			 (dns-make-network-process (car dns-servers))
		       (error
			(message "dns: Got an error while trying to talk to %s"
				 (car dns-servers))
			nil)))
	    (tcp-p (and (not (fboundp 'make-network-process))
			(not (featurep 'xemacs))))
	    (step 100)
	    (times (* dns-timeout 1000))
	    (id (random 65000)))
	(when process
	  (process-send-string
	   process
	   (dns-write `((id ,id)
			(opcode query)
			(queries ((,name (type ,type))))
			(recursion-desired-p t))
		      tcp-p))
	  (while (and (zerop (buffer-size))
		      (> times 0))
	    (accept-process-output process 0 step)
	    (decf times step))
	  (ignore-errors
	    (delete-process process))
	  (when (and tcp-p
		     (>= (buffer-size) 2))
	    (goto-char (point-min))
	    (delete-region (point) (+ (point) 2)))
	  (when (>= (buffer-size) 2)
	    (let ((result (dns-read (buffer-string))))
	      (if fullp
		  result
		(let ((answer (car (dns-get 'answers result))))
		  (when (eq type (dns-get 'type answer))
		    (dns-get 'data answer)))))))))))

(provide 'dns)

;;; arch-tag: d0edd0c4-4cce-4538-ae92-06c3356ee80a
;;; dns.el ends here
