;;; erc-compat.el --- ERC compatibility code for older Emacsen  -*- lexical-binding: t; -*-

;; Copyright (C) 2002-2003, 2005-2024 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; URL: https://www.emacswiki.org/emacs/ERC

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

;; This mostly defines stuff that cannot be worked around easily.

;; ERC depends on the `compat' library from GNU ELPA for supporting
;; older versions of Emacs.  See this discussion for additional info:
;; https://lists.gnu.org/archive/html/emacs-devel/2022-07/msg00512.html

;;; Code:

(require 'compat)
(eval-when-compile (require 'cl-lib))

(define-obsolete-function-alias 'erc-compat-function #'compat-function "30.1")
(define-obsolete-function-alias 'erc-compat-call #'compat-call "30.1")

;;;###autoload(autoload 'erc-define-minor-mode "erc-compat")
(define-obsolete-function-alias 'erc-define-minor-mode
  #'define-minor-mode "28.1")

(defun erc-decode-coding-string (s coding-system)
  "Decode S using CODING-SYSTEM."
  (declare (obsolete decode-coding-string "28.1"))
  (decode-coding-string s coding-system t))

(defun erc-encode-coding-string (s coding-system)
  "Encode S using CODING-SYSTEM.
Return the same string, if the encoding operation is trivial.
See `erc-encoding-coding-alist'."
  (declare (obsolete encode-coding-string "28.1"))
  (encode-coding-string s coding-system t))

(define-obsolete-function-alias 'erc-propertize #'propertize "28.1")
(define-obsolete-function-alias 'erc-view-mode-enter #'view-mode-enter "28.1")
(autoload 'help-function-arglist "help-fns")
(define-obsolete-function-alias 'erc-function-arglist #'help-function-arglist "28.1")
(define-obsolete-function-alias 'erc-delete-dups #'delete-dups "28.1")
(define-obsolete-function-alias 'erc-replace-regexp-in-string #'replace-regexp-in-string "28.1")

(defun erc-set-write-file-functions (new-val)
  (declare (obsolete nil "28.1"))
  (setq-local write-file-functions new-val))

(defvar erc-emacs-build-time
  (if (or (stringp emacs-build-time) (not emacs-build-time))
      emacs-build-time
    (format-time-string "%Y-%m-%d" emacs-build-time))
  "Time at which Emacs was dumped out, or nil if not available.")
(make-obsolete-variable 'erc-emacs-build-time 'emacs-build-time "28.1")
(define-obsolete-variable-alias 'erc-user-emacs-directory 'user-emacs-directory "28.1")

(defun erc-replace-match-subexpression-in-string
  (newtext string _match subexp _start &optional fixedcase literal)
  "Replace the subexpression SUBEXP of the last match in STRING with NEWTEXT.
MATCH is the text which matched the subexpression (see `match-string').
START is the beginning position of the last match (see `match-beginning').
See `replace-match' for explanations of FIXEDCASE and LITERAL."
  (declare (obsolete replace-match "28.1"))
  (replace-match newtext fixedcase literal string subexp))

(define-obsolete-function-alias 'erc-with-selected-window
  #'with-selected-window "28.1")
(define-obsolete-function-alias 'erc-cancel-timer #'cancel-timer "28.1")
(define-obsolete-function-alias 'erc-make-obsolete #'make-obsolete "28.1")
(define-obsolete-function-alias 'erc-make-obsolete-variable
  #'make-obsolete-variable "28.1")

;; Provide a simpler replacement for `cl-member-if'
(defun erc-member-if (predicate list)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches."
  (declare (obsolete cl-member-if "28.1"))
  (let ((ptr list))
    (catch 'found
      (while ptr
	(when (funcall predicate (car ptr))
	  (throw 'found ptr))
	(setq ptr (cdr ptr))))))

;; Provide a simpler replacement for `cl-delete-if'
(defun erc-delete-if (predicate seq)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
  (declare (obsolete cl-delete-if "28.1"))
  ;; remove from car
  (while (when (funcall predicate (car seq))
	   (setq seq (cdr seq))))
  ;; remove from cdr
  (let ((ptr seq)
	(next (cdr seq)))
    (while next
      (when (funcall predicate (car next))
	(setcdr ptr (if (consp next)
			(cdr next)
		      nil)))
      (setq ptr (cdr ptr))
      (setq next (cdr ptr))))
  seq)

;; Provide a simpler replacement for `cl-remove-if-not'
(defun erc-remove-if-not (predicate seq)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ to
avoid corrupting the original SEQ."
  (declare (obsolete cl-remove-if-not "28.1"))
  (let (newseq)
    (dolist (el seq)
      (when (funcall predicate el)
	(setq newseq (cons el newseq))))
    (nreverse newseq)))

;; Copied from cl-extra.el
(defun erc-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (declare (obsolete cl-subseq "28.1"))
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))


;;;; Auth Source

(declare-function auth-source-pass--get-attr
                  "auth-source-pass" (key entry-data))
(declare-function auth-source-pass--disambiguate
                  "auth-source-pass" (host &optional user port))
(declare-function auth-source-backend-parse-parameters
                  "auth-source-pass" (entry backend))
(declare-function auth-source-backend "auth-source" (&rest slots))
(declare-function auth-source-pass-entries "auth-source-pass" nil)
(declare-function auth-source-pass-parse-entry "auth-source-pass" (entry))

(defvar auth-sources)
(defvar auth-source-backend-parser-functions)

;; This hard codes `auth-source-pass-port-separator' to ":"
(defun erc-compat--29-auth-source-pass--retrieve-parsed (seen e port-number-p)
  (when (string-match (rx (or bot "/")
                          (or (: (? (group-n 20 (+ (not (in "/:")))) "@")
                                 (group-n 10 (+ (not (in "/:@"))))
                                 (? ":" (group-n 30 (+ (not (in " /:"))))))
                              (: (group-n 11 (+ (not (in "/:@"))))
                                 (? ":" (group-n 31 (+ (not (in " /:")))))
                                 (? "/" (group-n 21 (+ (not (in "/:")))))))
                          eot)
                      e)
    (puthash e `( :host ,(or (match-string 10 e) (match-string 11 e))
                  ,@(if-let* ((tr (match-string 21 e)))
                        (list :user tr :suffix t)
                      (list :user (match-string 20 e)))
                  :port ,(and-let* ((p (or (match-string 30 e)
                                           (match-string 31 e)))
                                    (n (string-to-number p)))
                           (if (or (zerop n) (not port-number-p))
                               (format "%s" p)
                             n)))
             seen)))

;; This looks bad, but it just inlines `auth-source-pass--find-match-many'.
(defun erc-compat--29-auth-source-pass--build-result-many
    (hosts users ports require max)
  "Return a plist of HOSTS, PORTS, USERS, and secret."
  (unless (listp hosts) (setq hosts (list hosts)))
  (unless (listp users) (setq users (list users)))
  (unless (listp ports) (setq ports (list ports)))
  (unless max (setq max 1))
  (let ((seen (make-hash-table :test #'equal))
        (entries (auth-source-pass-entries))
        (check (lambda (m k v)
                 (let ((mv (plist-get m k)))
                   (if (memq k require)
                       (and v (equal mv v))
                     (or (not v) (not mv) (equal mv v))))))
        out suffixed suffixedp)
    (catch 'done
      (dolist (host hosts)
        (pcase-let ((`(,_ ,u ,p) (auth-source-pass--disambiguate host)))
          (unless (or (not (equal "443" p)) (string-prefix-p "https://" host))
            (setq p nil))
          (dolist (user (or users (list u)))
            (dolist (port (or ports (list p)))
              (dolist (e entries)
                (when-let*
                    ((m (or (gethash e seen)
                            (erc-compat--29-auth-source-pass--retrieve-parsed
                             seen e (integerp port))))
                     ((equal host (plist-get m :host)))
                     ((funcall check m :port port))
                     ((funcall check m :user user))
                     (parsed (auth-source-pass-parse-entry e))
                     (secret (or (auth-source-pass--get-attr 'secret parsed)
                                 (not (memq :secret require)))))
                  (push
                   `( :host ,host ; prefer user-provided :host over h
                      ,@(and-let* ((u (plist-get m :user))) (list :user u))
                      ,@(and-let* ((p (plist-get m :port))) (list :port p))
                      ,@(and secret (not (eq secret t)) (list :secret secret)))
                   (if (setq suffixedp (plist-get m :suffix)) suffixed out))
                  (unless suffixedp
                    (when (or (zerop (cl-decf max))
                              (null (setq entries (delete e entries))))
                      (throw 'done out)))))
              (setq suffixed (nreverse suffixed))
              (while suffixed
                (push (pop suffixed) out)
                (when (zerop (cl-decf max))
                  (throw 'done out))))))))
    (reverse out)))

(cl-defun erc-compat--29-auth-source-pass-search
    (&rest spec &key host user port require max &allow-other-keys)
  ;; From `auth-source-pass-search'
  (cl-assert (and host (not (eq host t)))
             t "Invalid password-store search: %s %s")
  (let ((rv (erc-compat--29-auth-source-pass--build-result-many
             host user port require max)))
    (if (and (fboundp 'auth-source--obfuscate)
             (fboundp 'auth-source--deobfuscate))
        (let (out)
          (dolist (e rv out)
            (when-let* ((s (plist-get e :secret))
                        (v (auth-source--obfuscate s)))
              (setq e (plist-put e :secret (apply-partially
                                            #'auth-source--deobfuscate v))))
            (push e out)))
      rv)))

(defun erc-compat--29-auth-source-pass-backend-parse (entry)
  (when (eq entry 'password-store)
    (auth-source-backend-parse-parameters
     entry (auth-source-backend
            :source "."
            :type 'password-store
            :search-function #'erc-compat--29-auth-source-pass-search))))

(defun erc-compat--auth-source-backend-parser-functions ()
  (if (memq 'password-store auth-sources)
      (progn
        (require 'auth-source-pass)
        `(,@(unless (bound-and-true-p auth-source-pass-extra-query-keywords)
              '(erc-compat--29-auth-source-pass-backend-parse))
          ,@auth-source-backend-parser-functions))
    auth-source-backend-parser-functions))


;;;; SASL

(declare-function sasl-step-data "sasl" (step))
(declare-function sasl-error "sasl" (datum))
(declare-function sasl-client-property "sasl" (client property))
(declare-function sasl-client-set-property "sasl" (client property value))
(declare-function sasl-mechanism-name "sasl" (mechanism))
(declare-function sasl-client-name "sasl" (client))
(declare-function sasl-client-mechanism "sasl" (client))
(declare-function sasl-read-passphrase "sasl" (prompt))
(declare-function sasl-unique-id "sasl" nil)
(declare-function decode-hex-string "hex-util" (string))
(declare-function rfc2104-hash "rfc2104" (hash block-length hash-length
                                               key text))
(declare-function sasl-scram--client-first-message-bare "sasl-scram-rfc"
                  (client))
(declare-function cl-mapcar "cl-lib" (cl-func cl-x &rest cl-rest))

(defun erc-compat--29-sasl-scram-construct-gs2-header (client)
  (let ((authzid (sasl-client-property client 'authenticator-name)))
    (concat "n," (and authzid "a=") authzid ",")))

(defun erc-compat--29-sasl-scram-client-first-message (client _step)
  (let ((c-nonce (sasl-unique-id)))
    (sasl-client-set-property client 'c-nonce c-nonce))
  (concat (erc-compat--29-sasl-scram-construct-gs2-header client)
          (sasl-scram--client-first-message-bare client)))

(defun erc-compat--29-sasl-scram--client-final-message
    (hash-fun block-length hash-length client step)
  (unless (string-match
           "^r=\\([^,]+\\),s=\\([^,]+\\),i=\\([0-9]+\\)\\(?:$\\|,\\)"
           (sasl-step-data step))
    (sasl-error "Unexpected server response"))
  (let* ((hmac-fun
          (lambda (text key)
            (decode-hex-string
             (rfc2104-hash hash-fun block-length hash-length key text))))
         (step-data (sasl-step-data step))
         (nonce (match-string 1 step-data))
         (salt-base64 (match-string 2 step-data))
         (iteration-count (string-to-number (match-string 3 step-data)))
         (c-nonce (sasl-client-property client 'c-nonce))
         (cbind-input
          (if (string-prefix-p c-nonce nonce)
              (erc-compat--29-sasl-scram-construct-gs2-header client) ; *1
            (sasl-error "Invalid nonce from server")))
         (client-final-message-without-proof
          (concat "c=" (base64-encode-string cbind-input t) "," ; *2
                  "r=" nonce))
         (password
          (sasl-read-passphrase
           (format "%s passphrase for %s: "
                   (sasl-mechanism-name (sasl-client-mechanism client))
                   (sasl-client-name client))))
         (salt (base64-decode-string salt-base64))
         (string-xor (lambda (a b)
                       (apply #'unibyte-string (cl-mapcar #'logxor a b))))
         (salted-password (let ((digest (concat salt (string 0 0 0 1)))
                                (xored nil))
                            (dotimes (_i iteration-count xored)
                              (setq digest (funcall hmac-fun digest password))
                              (setq xored (if (null xored)
                                              digest
                                            (funcall string-xor xored
                                                     digest))))))
         (client-key (funcall hmac-fun "Client Key" salted-password))
         (stored-key (decode-hex-string (funcall hash-fun client-key)))
         (auth-message (concat "n=" (sasl-client-name client)
                               ",r=" c-nonce "," step-data
                               "," client-final-message-without-proof))
         (client-signature (funcall hmac-fun
                                    (encode-coding-string auth-message 'utf-8)
                                    stored-key))
         (client-proof (funcall string-xor client-key client-signature))
         (client-final-message
          (concat client-final-message-without-proof ","
                  "p=" (base64-encode-string client-proof t)))) ; *3
    (sasl-client-set-property client 'auth-message auth-message)
    (sasl-client-set-property client 'salted-password salted-password)
    client-final-message))


;;;; Misc 29.1

(defvar url-irc-function)
(declare-function url-type "url-parse" (cl-x))

(defun erc-compat--29-browse-url-irc (string &rest _)
  (require 'url-irc)
  (let* ((url (url-generic-parse-url string))
         (url-irc-function
          (if (eq url-irc-function 'url-irc-erc)
              (lambda (host port chan user pass)
                (erc-handle-irc-url host port chan user pass (url-type url)))
            url-irc-function)))
    (url-irc url)))

(cond ((fboundp 'browse-url-irc)) ; 29
      ((boundp 'browse-url-default-handlers) ; 28
       (add-to-list 'browse-url-default-handlers
                    '("\\`irc6?s?://" . erc-compat--29-browse-url-irc)
                    nil (lambda (_ a)
                          (and (stringp (car-safe a))
                               (string-match-p (car a) "irc://localhost")))))
      ((boundp 'browse-url-browser-function) ; 27
       (require 'browse-url)
       (let ((existing browse-url-browser-function))
         (setq browse-url-browser-function
               (if (functionp existing)
                   (lambda (u &rest r)
                     (apply (if (string-match-p "\\`irc6?s?://" u)
                                #'erc-compat--29-browse-url-irc
                              existing)
                            u r))
                 (cons '("\\`irc6?s?://" . erc-compat--29-browse-url-irc)
                       existing))))))

;; We can't store (TICKS . HZ) style timestamps on 27 and 28 because
;; `time-less-p' and friends do
;;
;;   message("obsolete timestamp with cdr ...", ...)
;;   decode_lisp_time(_, WARN_OBSOLETE_TIMESTAMPS, ...)
;;   lisp_time_struct(...)
;;   time_cmp(...)
;;
;; which spams *Messages* (and stderr when running the test suite).
(defmacro erc-compat--current-lisp-time ()
  "Return `current-time' as a (TICKS . HZ) pair on 29+."
  (if (>= emacs-major-version 29)
      '(let (current-time-list) (current-time))
    '(current-time)))

(defmacro erc-compat--defer-format-spec-in-buffer (&rest spec)
  "Transform SPEC forms into functions that run in the current buffer.
For convenience, ensure function wrappers return \"\" as a
fallback."
  (cl-check-type (car spec) cons)
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (current-buffer)))
       ,(list '\`
              (mapcar
               (pcase-lambda (`(,k . ,v))
                 (cons k
                       (list '\,(if (>= emacs-major-version 29)
                                    `(lambda ()
                                       (or (if (eq ,buffer (current-buffer))
                                               ,v
                                             (with-current-buffer ,buffer
                                               ,v))
                                           ""))
                                  `(or ,v "")))))
               spec)))))

(provide 'erc-compat)

;;; erc-compat.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
