;;; with-url.el --- High-Level URL Interface -*- lexical-binding: t -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: http url

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

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'puny)
(require 'gnutls)
(require 'mm-url)
(require 'url-http)
(require 'mail-parse)
(require 'subr-x)

(cl-defstruct url-request
  original-url wait timeout read-timeout
  verbose debug cookies cache ignore-errors
  headers
  method
  data data-charset data-encoding
  callback redirect-times
  url parsed-url process
  response-size start-time last-read-time timer
  finished follow-redirects buffer)

(defvar with-url-debug nil
  "If non-nil, record all actions in the \"*url-debug*\" buffer.")

(defvar with-url--headers nil)
(defvar with-url--status nil)

(cl-defmacro with-fetched-url ((url
                                &key wait timeout
                                read-timeout
                                (verbose 5)
                                (cookies t)
                                (cache t)
                                (follow-redirects t)
                                debug
                                headers
                                ignore-errors
                                (method ''get)
                                data
                                (data-charset ''utf-8)
                                data-encoding)
                               &body body)
  "Retrieve URL and execute BODY with point in a buffer with the response.

Example:

  (with-url (headers \"http://fsf.org/\")
    (message \"The size of the FSF front page is %s\" (buffer-size)))

The buffer is killed after BODY has exited.

Additional keywords can be given to `with-url' to alter its operation.

The returned headers can be examined with the `url-header'
function; the full status with the `url-status' function, and
whether the request returned as expected with the `url-okp' or
`url-errorp' functions.

:wait t
Normal `with-url' operation is asynchronous.  If this parameter
is given, the retrieval will be synchronous instead.  Not all
URLs support asynchronous operation.  In particular, file: and
ftp: documents will always be fetchedh synchronously.

:timeout SECONDS
Give up after approximately SECONDS seconds and execute BODY.

:read-timeout SECONDS
If no data has been received for the last SECONDS seconds, give
up and execute BODY.

:verbose NUMBER
The level of verbosity during operations.  0 will men no messages
are issued.

:debug BOOL
If non-nil, a buffer called \"*url-debug*\" will be created, and
all network traffic, both request and response, is copied to that
buffer.  This buffer may grow very large.

:ignore-errors BOOL
If non-nil, the body will not be executed if the contents
specified by the URL could not be fetched.

:follow-redirects BOOL
If non-nil (which is the default), follow HTTP redirects until
the final document is reached.

:cookies t/read/write/nil
If nil, cookies will neither be sent nor stored.  If `read',
cookies will be recorded, but not sent.  If `write', cookies will
be sent, but not stored.  If nil, no cookie handling will occur.

:headers ALIST
Add ALIST to the headers sent over to the server.  This should typically
look like

  ((\"User-Agent\" \"Emacs\"))

If the header name is the same as one of the automatically
generated headers, the value from this list will override the
automatically generated header.  To disable the header
completely, use nil as the value.

Additional elements in this alist are interpreted as the coding
system (defaulting to `utf-8') and the encoding
method (defaulting to `url-encode').

:method SYMBOL
The method to use for retrieving an HTTP(S) resource.  This defaults
to `get', and other popular values are `post', `update' and `put'.

:data STRING/ALIST
Data to include in the body of the HTTP(S) request when using
POST, UPDATE or PUT.  This can either be a string or an alist of POST values
on this form:

  '((\"NAME\" \"VALUE\")
    (\"submit\")
    ((\"NAME1\" \"VALUE1\")
     (\"NAME2\" \"VALUE2\")))

Elements with several values only make sense with the `multipart'
encoding (see below).

:data-charset CHARSET
What charset (i.e., encoded character set) this data should be
encoded as.  This defaults to `utf-8'.

:data-encoding ENCODING
When using the posting methods, the data is usually encoded in
some fashion.  Supported encodings are `url-form', `multipart'
and `base64'."
  (declare (indent 1))
  (let ((requestv (cl-gensym "request"))
        (buffer (cl-gensym "buffer")))
    `(let ((,requestv
            (make-url-request :original-url ,url
                              :timeout ,timeout
                              :read-timeout ,read-timeout
                              :verbose ,verbose
                              :debug ,debug
                              :cookies ,cookies
                              :cache ,cache
                              :headers ,headers
                              :method ,method
                              :ignore-errors ,ignore-errors
                              :data ,data
                              :data-charset ,data-charset
                              :data-encoding ,data-encoding
                              :start-time (current-time)
                              :last-read-time (current-time)
                              :follow-redirects ,follow-redirects
                              :redirect-times 0)))
       ,(if wait
            `(progn
               (with-url--wait ,requestv)
               (let ((,buffer (url-request-buffer ,requestv)))
                 (with-current-buffer ,buffer
                   (unwind-protect
                       (if (and (url-request-ignore-errors ,requestv)
                                (url-errorp))
                           (kill-buffer buffer)
                         (goto-char (point-min))
                         ,@body)
                     (kill-buffer ,buffer)))))
          `(progn
             (setf (url-request-callback ,requestv)
                   (lambda ()
                     ,@body))
             (with-url--fetch ,requestv))))))

(defun url-header (name &optional buffer)
  "Return the value of the specified URL header name from the current buffer.
Example use:

  (url-header 'content-length)

If given, return the value in BUFFER instead."
  (with-current-buffer (or buffer (current-buffer))
    (cdr (assq name with-url--headers))))

(defun url-status (name &optional buffer)
  "Return the status of the URL request in the current buffer.
If given, return the value in BUFFER instead."
  (with-current-buffer (or buffer (current-buffer))
    (cdr (assq name with-url--status))))

(defun url-okp (&optional buffer)
  "Return non-nil if the document was retrieved.
If given, return the value in BUFFER instead."
  (let ((status (url-status 'response buffer)))
    (and status
         (consp status)
         (numberp (car status))
         (<= 200 (car status) 299))))

(defun url-errorp (&optional buffer)
  "Say whether there was an error when retrieving the document.
If given, return the value in BUFFER instead."
  (not (url-okp buffer)))

(defun with-url--fetch (req)
  (unless (url-request-url req)
    (setf (url-request-url req) (url-request-original-url req)))
  (setf (url-request-parsed-url req)
        (url-generic-parse-url (url-request-url req)))
  (pcase (url-type (url-request-parsed-url req))
    ((or "http" "https") (with-url--fetch-http req))
    ("ftp" (with-url--fetch-ftp req))
    ("file" (with-url--fetch-file req))
    ("data" (with-url--fetch-data req))
    (_ (with-current-buffer (generate-new-buffer "*request*")
         (setf (url-request-buffer req) (current-buffer))
         (with-url--callback nil '(500 "Unsupported URL") req)))))

(defun with-url--fetch-http (req)
  (when (or (url-request-timeout req)
            (url-request-read-timeout req)))
  (with-current-buffer (generate-new-buffer "*request*")
    (set-buffer-multibyte nil)
    (setf (url-request-buffer req) (current-buffer))
    (if (and (memq (url-request-cache req) '(t read))
             (with-url-get-cache (url-request-url req)))
        ;; If we have the document in the cache, then just serve it out.
        (progn
          (goto-char (point-min))
          (insert "HTTP/1.1 200 Retrieved from cache\n")
          (with-url--parse-headers)
          (goto-char (point-min))
          (delete-region (point) (search-forward "\n\n"))
          (setf (url-request-finished req) t)
          (with-url--possible-callback req))
      ;; If not, fetch it from the web.
      (let* ((coding-system-for-read 'binary)
             (coding-system-for-write 'binary)
             (process
              (make-network-process
               :name (url-request-url req)
               :buffer (current-buffer)
               :host (url-host (url-request-parsed-url req))
               :service (or (url-portspec (url-request-parsed-url req))
                            (if (equal (url-type (url-request-parsed-url req))
                                       "https")
                                443
                              80))
               :nowait t
               :plist (list :request req)
               :tls-parameters
               (and (equal (url-type (url-request-parsed-url req)) "https")
                    (cons 'gnutls-x509pki
                          (gnutls-boot-parameters
                           :hostname (puny-encode-string
                                      (url-host
                                       (url-request-parsed-url req))))))
               :sentinel #'with-url--sentinel
               :filter #'with-url--filter)))
        (setf (url-request-timer req)
              (run-at-time 1 1 (lambda ()
                                 (with-url--timer req))))
        (setf (url-request-process req) process)))))

(defun with-url--fetch-ftp (req)
  (let ((parsed (url-request-parsed-url req)))
    ;; Transform the URL into Tramp syntax and let it worry about it.
    (with-url--fetch-file
     (concat "/"
             (and (url-user parsed)
                  (format "%s@" (url-user parsed)))
             (url-host parsed)
             (and (url-port parsed)
                  (format "#%s" (url-port parsed)))
             ":"
             (url-filename parsed)))))

(defun with-url--fetch-file (req)
  (with-current-buffer (generate-new-buffer "*request*")
    (set-buffer-multibyte nil)
    (setf (url-request-buffer req) (current-buffer))
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary))
      (condition-case err
          (insert-file-contents-literally
           (url-filename (url-request-parsed-url req)))
        (error
         (push (list 'response
                     500 (format "Error occurred while fetching file: %s" err))
               with-url--status)))
      (with-url--possible-callback req))))

(defun with-url--possible-callback (req)
  (goto-char (point-min))
  (let ((buffer (current-buffer)))
    (when (url-request-callback req)
      (if (and (url-request-ignore-errors req)
               (url-errorp))
          (kill-buffer buffer)
        (unwind-protect
            (funcall (url-request-callback req))
          (kill-buffer buffer))))))

(defun with-url--fetch-data (req)
  (with-current-buffer (generate-new-buffer "*request*")
    (set-buffer-multibyte nil)
    (let ((url (url-request-url req)))
      (when (string-match "\\`data:\\([^;,]*\\)\\(;\\([^,]+\\)\\)?,\\(.*\\)"
                          url)
        (let ((content-type (or (match-string 1 url) "text/plain"))
              (encoding (or (match-string 3 url) "base64")))
          (insert (match-string 4 url))
          (when (equal encoding "base64")
            (condition-case nil
                (base64-decode-region (point-min) (point-max))
              (error
               (setq with-url--status
                     '((response 500 "Invalid data"))))))
          (unless with-url--status
            (setq with-url--headers `((content-type . ,content-type))
                  with-url--status '((response 200 "OK"))))))
      (with-url--possible-callback req))))

(defun with-url--timer (req)
  (let ((now (float-time)))
    ;; There are two possible timeouts: One for the overall time of
    ;; the entire request...
    (when (or (and (url-request-timeout req)
                   (> (- now (float-time (url-request-start-time req)))
                      (url-request-timeout req)))
              ;; ... and one that's updated whenever new data arrives from the
              ;; server.
              (and (url-request-read-timeout req)
                   (> (- now (float-time (url-request-last-read-time req)))
                      (url-request-read-timeout req))))
      (with-url--callback (url-request-process req)
                                  '(500 "Timer expired")))))

(defun with-url--sentinel (process change)
  (let ((req (plist-get (process-plist process) :request)))
    (pcase change
      ("open\n"
       (with-url--send-request process))
      ("connection broken by remote peer\n"
       ;; We'll be in this situation if the peer closes the
       ;; connection.  If we ourselves have killed the connection,
       ;; then `url-request-finished' will be set.
       (unless (url-request-finished req)
         ;; If we have headers, and there's no content-length there,
         ;; nor any chunked encoding, then we may have gotten the
         ;; complete document anyway.
         (with-current-buffer (process-buffer process)
           (if (with-url--unexpected-early-close)
               (with-url--process-reply process)
             ;; Nope, it's an error.
             (with-url--callback
              process (list 500 (format "Peer closed connection: %s"
                                        (process-status process))))))))
      ("deleted\n"
       ;; We ignore these, as that's what happens when we end the
       ;; connection ourselves.
       )
      (_ (with-url--callback
          process (list 500 (format "Network error: %s"
                                    (replace-regexp-in-string "\n" "" change)))
          req)))))

(defun with-url--unexpected-early-close ()
  (goto-char (point-min))
  (when-let ((header-end (re-search-forward "\r?\n\r?\n" nil t)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (and (not (re-search-forward "content-length: *\\([0-9]+\\)"
                                   header-end t))
           (not (re-search-forward "Transfer-Encoding: *chunked"
                                   header-end t))))))

(defun with-url--send-request (process)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((req (plist-get (process-plist process) :request))
           (parsed (url-request-parsed-url req)))
      (insert (format "%s %s%s HTTP/1.1\r\n"
                      (upcase (symbol-name (url-request-method req)))
                      (if (zerop (length (url-filename parsed)))
                          "/"
                        (url-filename parsed))
                      (if (and (eq (url-request-method req) 'get)
                               (url-request-data req))
                          (concat "?" (cl-caddr
                                       (with-url--data req 'url-encode)))
                        "")))
      (let* ((data (with-url--data req))
             (headers
              (list
               (list 'user-agent (url-http-user-agent))
               (list 'connection "close")
               (list 'accept-encoding
                     (and (fboundp 'zlib-available-p)
                          (zlib-available-p)
                          nil
                          "gzip"))
               (list 'accept "*/*")
               (list 'content-type (car data))
               (list 'content-transfer-encoding (cadr data))
               (list 'content-length (length (cl-caddr data)))
               (list 'cookies
                     (and (memq (url-request-cookies req) '(t write))
                          (with-url--cookies parsed)))
               (list 'host (url-host parsed))
               (list 'if-modified-since
                     (and (memq (url-request-cache req) '(t write))
                          (with-url-cache-time (url-request-url req)))))))
        ;; First insert automatically generated headers (unless we've
        ;; given explicit headers that override them).
        (dolist (elem headers)
          (when (and (cadr elem)
                     (not (assq (car elem) (url-request-headers req))))
            (with-url--insert-header elem)))
        ;; Then insert the explicitly given headers.
        (dolist (elem (url-request-headers req))
          (when (cadr elem)
            (with-url--insert-header elem)))
        (insert "\r\n")
        (when data
          (insert (cl-caddr data)))
        (when (or (url-request-debug req)
                  with-url-debug)
          (with-url--debug 'request (buffer-string)))))
    (process-send-region process (point-min) (point-max))))

(defvar with-url--header-defaults
  ;; Name Charset Encoding
  '((host nil puny)))

(defun with-url--insert-header (header)
  (let* ((name (car header))
         (defaults (cdr (assq name with-url--header-defaults)))
         (charset (cond
                   ((nthcdr 2 header)
                    (nth 2 header))
                   (defaults
                    (car defaults))
                   (t
                    'utf-8)))
         (encoding (or (nth 3 header) (nth 1 defaults)))
         (value (nth 1 header)))
    ;; Allow symbols and numbers as values for convenience.
    (unless (stringp value)
      (setq value (format "%s" value)))
    (when charset
      (setq value (encode-coding-string value charset)))
    (insert (capitalize (symbol-name name)) ": ")
    (insert (pcase encoding
              (`puny (puny-encode-string value))
              (`base64 (base64-encode-string value t))
              (`url-encode (url-hexify-string value))
              (_ value)))
    (insert "\r\n")))

(defun with-url--debug (type string)
  (with-current-buffer (get-buffer-create "*url-debug*")
    (goto-char (point-max))
    (insert (if (eq type 'request)
                ">>> "
              "<<< ")
            (format-time-string "%Y%m%dT%H:%M:%S") "\n"
            string)
    (unless (bolp)
      (insert "\n"))
    (insert "----------\n")))

(defun with-url--data (req &optional encoding)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when-let ((data (url-request-data req)))
      (cl-case (or encoding
                   (url-request-data-encoding req))
        (url-encode
         (list "application/x-www-form-urlencoded"
               nil
               (if (stringp data)
                   (encode-coding-string data (url-request-data-charset req))
                 (mm-url-encode-www-form-urlencoded data))))
        (multipart
         (let ((boundary (mml-compute-boundary '())))
           (list (concat "multipart/form-data; boundary=" boundary)
                 nil
                 (mm-url-encode-multipart-form-data values boundary))))
        (base64
         (if (stringp (url-request-data req))
             (insert (encode-coding-string data (url-request-data-charset req)))
           (mm-url-encode-www-form-urlencoded data))
         (base64-encode-region (point-min) (point-max))
         (list "application/x-www-form-urlencoded"
               "base64"
               (buffer-string)))))))

(defun with-url--filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (let ((req (plist-get (process-plist process) :request)))
      (setf (url-request-last-read-time req) (current-time))
      ;; Check whether we've got all the data.  We may already have
      ;; saved the response size.
      (unless (url-request-response-size req)
        ;; Get it the hard way.
        (goto-char (point-min))
        (save-match-data
          (let ((case-fold-search t))
            (when-let ((header-end (re-search-forward "^\r?\n" nil t)))
              (goto-char (point-min))
              ;; Only search until header-end since there may be no
              ;; Content-Length header here and we don't want to
              ;; search the contents.
              (cond
               ;; Content-Length header that says what the size is.
               ((re-search-forward "content-length: *\\([0-9]+\\)"
                                   header-end t)
                (let ((size (string-to-number (match-string 1))))
                  (setf (url-request-response-size req)
                        ;; The buffer should end up being the size of
                        ;; the headers plus the body.
                        (+ header-end size -1))))
               ;; No Content-Length; instead the data is passed in
               ;; chunks.
               ((re-search-forward "Transfer-Encoding: *chunked" header-end t)
                (goto-char header-end)
                ;; This could be sped up by looking at the end of the
                ;; buffer and see whether there's a 0 length block
                ;; there instead of traversing the entire buffer
                ;; (which may be slow on big documents).
                (let (length)
                  (while (looking-at "\\([0-9A-Za-z]+\\)\r?\n")
                    (setq length (string-to-number (match-string 1) 16))
                    (forward-line)
                    (if (zerop length)
                        (setf (url-request-response-size req) (buffer-size))
                      ;; Skip ahead, and then past the CRLF.
                      (goto-char (+ (point) length 2)))))))))))
      (when (and (url-request-response-size req)
                 (>= (buffer-size) (url-request-response-size req)))
        (with-url--process-reply process)))))

(defun with-url--process-reply (process)
  (with-url--parse-headers)
  (let* ((code (car (url-status 'response)))
         (req (plist-get (process-plist process) :request))
         (status (cadr (assq code url-http-codes))))
    ;; Set cookies (if the caller has requested that we record
    ;; cookies, and we've gotten some).
    (when (and (memq (url-request-cookies req) '(t read))
               (url-header 'cookie))
      (url-cookie-handle-set-cookie (url-header 'cookie)))
    (when (or (url-request-debug req)
              with-url-debug)
      (with-url--debug 'response (buffer-string)))
    (cond
     ;; We got the expected response.
     ((<= 200 code 299)
      (with-url--callback process))
     ;; We don't support proxies.
     ((eq status 'use-proxy)
      (with-url--callback
       process '(500 (format
                      "Redirection through proxy server not supported: %s"
                      (url-header 'location)))))
     ;; The document is in the cache.
     ((eq status 'not-modified)
      (with-url-get-cache (url-request-url req))
      (with-url--parse-headers)
      (with-url--callback process))
     ;; Redirects.
     ((<= 300 code 399)
      (cl-incf (url-request-redirect-times req))
      (when (memq code '(302 307))
        (setf (url-request-method req) 'get))
      (cond
       ((not (url-request-follow-redirects req))
        (with-url--callback process '(200 "Redirect not followed")))
       ((> (url-request-redirect-times req) 10)
        (with-url--callback process '(500 "Too many redirections")))
       (t
        (with-url--redirect process
                            (url-expand-file-name
                             (url-header 'location) (url-request-url req))))))
     (t
      (with-url--callback process)))))

(defun with-url--callback (process &optional status req)
  (let ((req (or req (plist-get (process-plist process) :request))))
    (with-current-buffer (url-request-buffer req)
      (setf (url-request-finished req) t)
      ;; Pass the https certificate on to the caller.
      (when process
        (when (gnutls-available-p)
          (push (cons 'tls-peer (gnutls-peer-status process))
                with-url--status))
        (delete-process process)
        (set-process-sentinel process nil)
        (set-process-filter process nil))
      (when (url-request-timer req)
        (cancel-timer (url-request-timer req)))
      (push (cons 'url (url-request-url req)) with-url--status)
      ;; Allow overriding the status if we have a timeout or the like.
      (when status
        (push (cons 'response status) with-url--status))
      ;; Delete the headers from the buffer.
      (goto-char (point-min))
      (when (re-search-forward "^\r?\n" nil t)
        (delete-region (point-min) (point)))
      ;; If we have a chunked transfer encoding, then we have to
      ;; remove the chunk length indicators from the response.
      (when (cl-equalp (url-header 'transfer-encoding) "chunked")
        (with-url--decode-chunked))
      ;; The contents may be compressed.
      (when (and (cl-equalp (url-header 'content-encoding) "gzip")
                 (fboundp 'zlib-available-p)
                 (zlib-available-p))
        (zlib-decompress-region (point-min) (point-max)))
      ;; Text responses should have the CRLF things removed.
      (when (string-match "^text/" (or (url-header 'content-type)
                                       "text/html"))
        (goto-char (point-min))
        (while (search-forward "\r\n" nil t)
          (forward-char -1)
          (delete-char -1)))
      (when (and (memq (url-request-cache req) '(t write))
                 (eq (url-request-method req) 'get)
                 (url-okp))
        (with-url-put-cache (url-request-url req)))
      (with-url--possible-callback req))))

(defun with-url--decode-chunked ()
  (let (length)
    (goto-char (point-min))
    (while (looking-at "\\([0-9A-Za-z]+\\)\r?\n")
      (setq length (string-to-number (match-string 1) 16))
      (forward-line)
      (delete-region (match-beginning 0) (point))
      (if (zerop length)
          (delete-region (match-beginning 0) (point-max))
        ;; Skip ahead.
        (goto-char (+ (point) length))
        ;; Delete the CRLF.
        (delete-char 2)))))

(defun with-url--redirect (process location)
  (let ((req (plist-get (process-plist process) :request)))
    (setf (url-request-url req) location
          (url-request-parsed-url req) nil
          (url-request-response-size req) nil
          (url-request-finished req) nil)
    (set-process-sentinel process nil)
    (set-process-filter process nil)
    (when (url-request-timer req)
      (cancel-timer (url-request-timer req)))
    (delete-process process)
    (kill-buffer (process-buffer process))
    (with-url--fetch req)))

(defun with-url--cookies (parsed)
  (mapconcat
   (lambda (cookie)
     (format "%s=%s" (url-cookie-name cookie) (url-cookie-value cookie)))
   ;; Have to sort this for sending most specific cookies first.
   (sort (url-cookie-retrieve (url-host parsed)
                              (url-filename parsed)
                              (equal (url-type parsed) "https"))
         (lambda (cookie1 cookie2)
           (> (length (url-cookie-localpart cookie1))
              (length (url-cookie-localpart cookie2)))))
   "; "))

(defun with-url--parse-headers ()
  (goto-char (point-min))
  (setq with-url--status nil
        with-url--headers nil)
  (let ((headers nil))
    (while (not (looking-at "\r?$"))
      (cond
       ;; The first line is the status line.
       ((not with-url--status)
        ;; Well-formed status line.
        (push
         (cons 'response
               (if (looking-at "\\([^ \n]+\\) +\\([0-9]+\\) +\\([^\r\n]*\\)")
                  (list (string-to-number (match-string 2))
                        (match-string 3)
                        (match-string 1))
                ;; Non-well-formed status line.
                (buffer-substring
                 (point)
                 (and (re-search-forward "\r?$")
                      (match-beginning 0)))))
         with-url--status))
       ;; Ignore all non-header lines in the header.
       ((looking-at "\\([^\r\n:]+\\): *\\([^\r\n]+\\)")
        (push (cons (intern (downcase (match-string 1)) obarray)
                    (match-string 2))
              headers)))
      (forward-line 1))
    (setq-local with-url--headers (nreverse headers))
    with-url--headers))

(defun with-url--wait (req)
  (prog1
      (with-url--fetch req)
    (while (not (url-request-finished req))
      (sleep-for 0.1))))

(defun with-url-put-cache (url)
  "Put the current buffer into a cache designated by URL.
If the headers don't allow caching, nothing will be done."
  ;; Use this opportunity to possibly prune the cache.
  (with-url--possibly-prune-cache)
  ;; We store things in the cache if they have a Last-Modified header
  ;; and they either don't have an Expires header, or it's in the
  ;; future.
  (let ((expires nil))
    (current-buffer)
    (when (and (or (url-header 'last-modified)
                   (and (url-header 'cache-control)
                        (setq expires (with-url--parse-cache-control
                                       (url-header 'cache-control)))))
               (or (not (url-header 'expires))
                   (progn
                     (setq expires
                           (ignore-errors
                             (apply #'encode-time
                                    (parse-time-string (url-header 'expires)))))
                     (or (not expires)
                         (time-less-p (current-time) expires)))))
      (let ((contents (buffer-string))
            (buffer (current-buffer)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert "Content-Type: " (or (url-header 'content-type buffer)
                                       "text/plain")
                  "\n")
          (when (url-header 'last-modified buffer)
            (insert "Last-Modified: " (url-header 'last-modified buffer) "\n"))
          ;; If there's no Expires header, we cache for one day.
          (insert "Expires: "
                  (let ((system-time-locale "C"))
                    (format-time-string "%a, %d %b %Y %T %z"
                                        (or expires
                                            (time-add (current-time)
                                                      (list 0 (* 60 60 24))))))
                  "\n")
          (insert "\n")
          (insert contents)
          (let ((file (with-url--cache-file-name url)))
            (unless (file-exists-p (file-name-directory file))
              (make-directory (file-name-directory file) t))
            (write-region (point-min) (point-max) file nil 'silent)))))))

(defun with-url--parse-cache-control (control)
  ;; Cache-Control: public, max-age=604800
  (when (string-match "max-age *= *\\([0-9]+\\)" control)
    (time-add (current-time) (seconds-to-time
                              (string-to-number (match-string 1 control))))))

(defun with-url-cache-time (url)
  "Return the Last-Modified timestamp for the cached version of URL, if any."
  (let ((file (with-url--cache-file-name url)))
    (when (file-exists-p file)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (narrow-to-region (point) (or (search-forward "\n\n" nil t) (point)))
        (mail-fetch-field "last-modified")))))

(defun with-url-cached-p (url)
  (file-exists-p (with-url--cache-file-name url)))

(defun with-url-get-cache (url)
  (let ((file (with-url--cache-file-name url)))
    (when (file-exists-p file)
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (if (not (with-url--cached-expired-p))
          t
        (erase-buffer)
        (ignore-errors
          (delete-file file))
        nil))))

(defun with-url--cached-expired-p ()
  (let ((expires
         (save-restriction
           (narrow-to-region
            (point) (or (search-forward "\n\n" nil t) (point)))
           (ignore-errors
             (apply #'encode-time
                    (parse-time-string
                     (mail-fetch-field "expires")))))))
    (or (null expires)
        (time-less-p expires (current-time)))))

(defvar with-url--last-prune-time nil)

(defun with-url--possibly-prune-cache ()
  "Prune the cache maximum once per hour."
  (when (and (file-exists-p
              (expand-file-name "url/cached" user-emacs-directory))
             (or (not with-url--last-prune-time)
                 (> with-url--last-prune-time (- (float-time) (* 60 60)))))
    (setq with-url--last-prune-time (float-time))
    (with-url--prune-cache)))

(defun with-url--prune-cache ()
  ;; We delete files that are older than a day.  It would perhaps be
  ;; nicer to actually look at expiration dates and stuff, but doing
  ;; so would be rather slow.  In any case, best current practice for
  ;; files without explicit Expires (etc) headers is to just store
  ;; them for a day, so it's OK.
  (let ((cutoff (time-subtract (current-time) (seconds-to-time (* 60 60 24)))))
    (dolist (file (directory-files-recursively
                   (expand-file-name "url/cached" user-emacs-directory)
                   "\\`[a-z0-9]+\\'"))
      (when (time-less-p
             (file-attribute-modification-time (file-attributes file)) cutoff)
        (ignore-errors
          (delete-file file))))))

(defun with-url--cache-file-name (url)
  "Return a file name appropriate to store URL.
It's based in `user-emacs-directory' and is hash-based, and is
several directories deep to avoid creating extremely large single
directories."
  (with-temp-buffer
    (insert (sha1 url))
    (goto-char (point-min))
    (insert (expand-file-name "url" user-emacs-directory) "/cached/")
    ;; We have a two-level directory structure with at most 256
    ;; top-level directories.
    (forward-char 2)
    (insert "/")
    (buffer-string)))

(provide 'with-url)

;;; with-url.el ends here
