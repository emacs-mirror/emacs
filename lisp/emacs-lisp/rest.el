;;; api.el --- library for interacting with restful web APIs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Keywords: comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)


;;; Error reporting
(defun api-report-buffer ()
  "Write TEXT to the *Api Server* buffer."
  (let ((text (buffer-string)))
    (with-current-buffer (get-buffer-create "*Api Report*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (goto-char (point-min)))))

(define-error 'api-error "Unkown api error, see \"*Api Report*\" buffer")
(define-error 'api-page-does-not-exist
  "This page doesn't seem to exist (see \"*Api Report*\" buffer), replied said"
  'api-error)
(define-error 'api-empty-redirect
  "Redirect received, but Location header not found! (see \"*Api Report*\" buffer)"
  'api-error)
(define-error 'api-unintelligible-result
  "Tried contacting server, but I can't understand the reply.  See \"*Api Report*\" buffer"
  'api-error)
(define-error 'api-bad-request
  "Server didn't understand my request, please you should probably file a bug report"
  'api-error)
(define-error 'api-unauthorized
  "Server says you're not authenticated"
  'api-error)
(define-error 'api-infinite-redirection-loop
  "Server is sending us in a redirection loop"
  'api-error)
(define-error 'api-server-error
  "Something bad happened on the server side, see \"*Api Report*\" buffer"
  'api-error)

(defun api-error (signal &rest args)
  "Throw an error SIGNAL with ARGS.
Also print contents of current buffer to *Api Report*."
  (declare (indent 1))
  (api-report-buffer)
  (signal signal args))

(defun api-parse-response-code (&optional is-auth)
  "Non-nil if this reponse buffer looks ok.
Leave point at the return code on the first line."
  (goto-char (point-min))
  (unless (search-forward-regexp "^HTTP/[.0-9]+ +" nil t)
    (api-error 'api-unintelligible-result))
  (pcase (thing-at-point 'number)
    ((or 100 204) nil)       ;; OK, but no content.
    ((or 200 202 201 203) t) ;; OK, with content.
    ;; Redirection
    ((or 301 302 303 304 305 307)
     (if (search-forward-regexp "^Location: *\\([^\s\n\r\t]+\\)" nil 'noerror)
         (match-string 1)
       (api-error 'api-empty-redirect)))
    ;; Client errors
    ((or 403 404 405 410) (api-error 'api-page-does-not-exist
                            (substring-no-properties (thing-at-point 'line) 0 -1)))
    ((or 400 422 408 409 411 412 413 414 415 416 417) (api-error 'api-bad-request))
    ((or 401 407) (api-error 'api-unauthorized
                    (if is-auth "try creating a new token"
                      "you probably need to configure a token")))
    ((pred (<= 500)) (api-error 'api-server-error))
    (_ (api-error 'api-error
         (substring (thing-at-point 'line) 0 -1)))))


;;; Requests
(autoload 'auth-source-search "auth-source")
(cl-defmacro api--with-server-buffer (method url &rest body &key async unwind-form
                                             extra-headers &allow-other-keys)
  "Run BODY in a Server request buffer.
UNWIND-FORM is run no matter what, and doesn't affect the return
value."
  (declare (indent 2)
           (debug t))
  (let ((call-name (make-symbol "callback"))
        (secret (make-symbol "secret")))
    (while (keywordp (car body))
      (setq body (cdr (cdr body))))
    `(let ((,secret (when ,auth
                      (if (listp ,auth)
                          (or (car-safe (apply #'auth-source-search
                                               :require '(:secret) :max 1
                                               ,auth))
                              (user-error "This request requires authentication"))
                        (lambda () ,auth))))
           (,call-name (lambda (status)
                         (unwind-protect
                             (progn (when-let ((er (plist-get status :error)))
                                      (error "Error retrieving: %s %S" ,url er))
                                    ,@body)
                           ,unwind-form
                           (kill-buffer (current-buffer))))))
       (setq method (upcase (replace-regexp-in-string
                             "\\`:" "" (format "%s" method))))
       (let ((url-request-method ,method)
             (url-request-extra-headers
              (cons '("Content-Type" . "application/x-www-form-urlencoded")
                    ,extra-headers)))
         (if ,async
             (condition-case error-data
                 (url-retrieve ,url ,call-name nil 'silent)
               (error ,unwind-form
                      (signal (car error-data) (cdr error-data))))
           (let ((buffer (condition-case error-data
                             (url-retrieve-synchronously ,url 'silent)
                           (error ,unwind-form
                                  (signal (car error-data) (cdr error-data))))))
             (with-current-buffer buffer
               (funcall ,call-name nil))))))))

(defvar-local api-root nil
  "Prepended to api method when a full url is not given.")

(defun api--headers-alist ()
  "Return an alist of all headers above point."
  (let ((ac))
    (while (search-backward-regexp "^\\(X-[^ ]+\\): *\\(.*?\\)\r?$" nil 'noerror)
      (push (cons (intern (downcase (match-string 1))) (match-string 2))
            ac))
    ac))

(autoload 'json-read "json")

(defvar api--url-depth nil
  "Used to detect infinite redirection loops.")


;;; Authentication
(defun api--auth-source-search (url-obj)
  "Return authentication information for URL-OBJ.
URL-OBJ is a value returned by `url-generic-parse-url'.
Information is found by running `auth-source-search' with the
properties of URL-OBJ."
  (let ((type (url-type url-obj))
        (port (url-port url-obj))
        (args (list :require '(:secret) :host (url-host url-obj)
                    :max 1 :user (url-user url-obj))))
    (car (or (apply #'auth-source-search :port port :type type args)
             (apply #'auth-source-search :port port args)
             ;; If URL does not specify a port, try again without the default.
             (unless (url-portspec url-obj)
               (or (apply #'auth-source-search :type type args)
                   (apply #'auth-source-search args)))))))

(defun api--get-auth-info (info)
  "Return a function that returns (USER . PASSWORD).
INFO is a plist returned by `auth-source-search'."
  (let ((user (plist-get info :user))
        (pass (plist-get info :secret)))
    (lambda () (cons user (funcall pass)))))

(defun api--make-authorization-header (_plist user password)
  "Return an alist containing an \"Authorization\" header.
The car of the list is nil, so this function can be used as the
AUTH-METHOD in `api-action'."
  `(nil . (("Authorization" . ,(concat "Basic "
                                       (base64-encode-string
                                        (concat user ":" password)))))))


;;; The function
;;;###autoload
(cl-defun api-action (url &rest all-options
                          &key auth
                          (method :get)
                          (reader #'json-read)
                          (callback #'identity)
                          async
                          (max-pages 1)
                          (next-page-rule '(header "Link"))
                          extra-headers
                          (auth-method (if auth #'api--make-authorization-header))
                          (return :simple)
                          -url-history)
  "Contact URL with METHOD.
METHOD is a keyword of an http method, defaulting to :get.

URL can be a string such as \"user/starred?per_page=100\" to
be appended at the end of `api-root'.  It can also be a full url
string, in which case it is used verbatim.

READER is called as a function with no arguments, with point
after the headers.  If MAX-PAGES > 1 is specified, then READER
must return a sequence.  READER is `json-read' by default.  Set it
to `ignore' if you don't care about the response data.  READER is
not called if the response had no content.

CALLBACK is a function that will be called with the data returned
by READER as an argument.  CALLBACK is called even if the response
was empty (in which case its argument is nil).

The return value depends on a few factors:
- If ASYNC is non-nil, the return value is undefined.
- Otherwise, return the value returned by CALLBACK (or by READER,
  if no CALLBACK provided).
- If RETURN is :rich, return a list.  The car is the value
  returned by CALLBACK, and the cdr is an alist of meta-data
  about the request \(next-page, quota, etc).

If ASYNC is non-nil, run the request asynchronously.

AUTH may have four forms, 2 and 3 may prompt for information.
1. nil (the default), meaning no authentication is done.
2. t, meaning a user/password combination is automatically obtained
   by running `auth-source-search' with the host and port.
3. A list of arguments to pass directly to `auth-source-search'.
4. A function that returns (\"USER\" . \"PASSWORD\") when called.

AUTH-METHOD determines how to use the authentication information.
By default, it does basic authentication with the \"Authorization\"
header.
If provided, it must be a function taking three arguments, which
should return a cons cell.  The car of this cell (if non-nil)
replaces URL and the cdr is appended to EXTRA-HEADERS.  It is
called with a plist, the user string and the password string.
The plist contais at least :url, :method, and :extra-headers.

`api-action' can also handle the pagination used in server
results by appending together the contents of each page.  Use
MAX-PAGES to increase the number of pages that are
fetched (default 1).

By default the URL of the next page is taken from the \"Link\"
header.  You can change this by passing somthing like
    (header \"Next-link\")
as the value of the NEXT-PAGE-PROPERTY keyword.  You can also pass
a regexp like this:
    (regexp \"Some \\(.*\\)regexp\")
which is then searched and `(match-string 1)' is used as the URL.

EXTRA-HEADERS is an alist from header names (string) to header
values (string), as per `url-request-extra-headers'.

If the http request is unsuccessful, an error is signaled
according to the reply.  The possible errors are:
`api-bad-request', `api-server-error', `api-unauthorized',
`api-unintelligible-result', `api-empty-redirect',
`api-page-does-not-exist', and `api-infinite-redirection-loop',
all of which inherit from `api-error'.

\(fn URL &key AUTH (METHOD :get) (READER #'json-read) CALLBACK ASYNC AUTH-METHOD (MAX-PAGES 1) NEXT-PAGE-RULE EXTRA-HEADERS RETURN)"
  (declare (indent 1))
  (unless (string-match "\\`https?://" url)
    (setq url (concat api-root url)))
  (when (member url -url-history)
    (signal 'api-infinite-redirection-loop (cons url api--url-depth)))
  (when auth
    (let ((href (url-generic-parse-url url)))
      (when (url-password href)
        (error "AUTH requested, but URL already contains a password"))
      (unless (functionp auth)
        (setq auth (api--get-auth-info (if (listp auth)
                                           (apply #'auth-source-search auth)
                                         (api--auth-source-search href)))))
      (pcase-let* ((`(,user . ,pass) (funcall auth))
                   (`(,new-url . ,headers)
                    (funcall auth-method (list :url url :method method
                                               :extra-headers extra-headers)
                             user pass)))
        (when new-url (setq url new-url))
        (setq extra-headers (append headers extra-headers)))))
  (api--with-server-buffer method url
    :extra-headers extra-headers
    :-url-depth (cons url -url-history)
    :async async
    (pcase (api-parse-response-code auth)
      (`nil nil)
      ((and (pred stringp) link)
       (message "Redirected to %s" link)
       (apply #'api-action all-options))
      (`t
       (let ((next-page
              (when (pcase next-page-rule
                      (`(header ,name) (search-forward-regexp
                                        (format "^%s: .*<\\([^>]+\\)>;" (regexp-quote name))
                                        nil t))
                      (`(regexp ,rx) (search-forward-regexp rx nil t))
                      (_ nil))
                (match-string-no-properties 1))))
         (goto-char (point-min))
         (search-forward-regexp "^\r?$")
         (let* ((data (unless (eobp) (funcall reader))))
           (if (or (not next-page)
                   (< max-pages 2))
               (pcase return
                 (:simple (funcall callback data))
                 (:rich `(,(funcall callback data)
                          (next-page . ,next-page)
                          ,@(api--headers-alist))))
             (api-action next-page
               :auth auth
               :method method
               :reader reader
               :next-page-rule next-page-rule
               :return return
               :async  async
               :max-pages (1- max-pages)
               :callback (lambda (res)
                           (funcall callback
                                    (if (listp res)
                                        (append data res)
                                      (vconcat data res))))))))))))

(provide 'api)
;;; api.el ends here
