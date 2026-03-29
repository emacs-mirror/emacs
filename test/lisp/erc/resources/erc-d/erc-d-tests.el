;;; erc-d-tests.el --- tests for erc-d -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (expand-file-name ".." (ert-resource-directory))
                         load-path)))
    (require 'erc-d)
    (require 'erc-d-t)))

(require 'erc)

;; Temporary kludge to silence warning
(put 'erc-parse-tags 'erc-v3-warned-p t)

(ert-deftest erc-d-u--canned-load-dialog--basic ()
  (should-not (get-buffer "basic.eld"))
  (should-not erc-d-u--canned-buffers)
  (let* ((exes (erc-d-u--canned-load-dialog 'basic))
         (reap (lambda ()
                 (cl-loop with e = (erc-d-u--read-dialog exes)
                          for s = (erc-d-u--read-exchange e)
                          while s collect s))))
    (should (get-buffer "basic.eld"))
    (should (memq (get-buffer "basic.eld") erc-d-u--canned-buffers))
    (should (equal (funcall reap) '((pass 10.0 "PASS " (? ?:) "changeme"))))
    (should (equal (funcall reap) '((nick 0.2 "NICK tester"))))
    (let ((r (funcall reap)))
      (should (equal (car r) '(user 0.2 "USER user 0 * :tester")))
      (should (equal
               (car (last r))
               '(0 ":irc.example.org 422 tester :MOTD File is missing"))))
    (should (equal (car (funcall reap)) '(mode-user 5 "MODE tester +i")))
    (should (equal (funcall reap)
                   '((mode-chan 3.2 "MODE #chan")
                     (0.1 ":bob!~bob@example.org PRIVMSG #chan :hey"))))
    ;; See `define-error' site for `iter-end-of-sequence'
    (ert-info ("EOB detected") (should-not (erc-d-u--read-dialog exes))))
  (should-not (get-buffer "basic.eld"))
  (should-not erc-d-u--canned-buffers))

(defun erc-d-tests--make-hunk-reader (hunks)
  (let ((p (erc-d-u--read-dialog hunks)))
    (lambda () (erc-d-u--read-exchange p))))

;; Fuzzies need to be able to access any non-exhausted genny.
(ert-deftest erc-d-u--canned-load-dialog--intermingled ()
  (should-not (get-buffer "basic.eld"))
  (should-not erc-d-u--canned-buffers)
  (let* ((exes (erc-d-u--canned-load-dialog 'basic))
         (pass (erc-d-tests--make-hunk-reader exes))
         (nick (erc-d-tests--make-hunk-reader exes))
         (user (erc-d-tests--make-hunk-reader exes))
         (modu (erc-d-tests--make-hunk-reader exes))
         (modc (erc-d-tests--make-hunk-reader exes)))

    (should (equal (funcall user) '(user 0.2 "USER user 0 * :tester")))
    (should (equal (funcall modu) '(mode-user 5 "MODE tester +i")))
    (should (equal (funcall modc) '(mode-chan 3.2 "MODE #chan")))

    (cl-loop repeat 8 do (funcall user)) ; skip a few
    (should (equal (funcall user)
                   '(0 ":irc.example.org 254 tester 1 :channels formed")))
    (should (equal (funcall modu)
                   '(0 ":irc.example.org 221 tester +Zi")))
    (should (equal (cl-loop for s = (funcall modc) while s collect s) ; done
                   '((0.1 ":bob!~bob@example.org PRIVMSG #chan :hey"))))

    (cl-loop repeat 3 do (funcall user))
    (cl-loop repeat 3 do (funcall modu))

    (ert-info ("Change up the order")
      (should
       (equal (funcall modu)
              '(0 ":irc.example.org 366 alice #chan :End of NAMES list")))
      (should
       (equal (funcall user)
              '(0 ":irc.example.org 422 tester :MOTD File is missing"))))

    ;; Exhaust these
    (should (equal (cl-loop for s = (funcall pass) while s collect s) ; done
                   '((pass 10.0 "PASS " (? ?:) "changeme"))))
    (should (equal (cl-loop for s = (funcall nick) while s collect s) ; done
                   '((nick 0.2 "NICK tester"))))

    (ert-info ("End of file but no teardown because hunks outstanding")
      (should-not (erc-d-u--read-dialog exes))
      (should (get-buffer "basic.eld")))

    ;; Finish
    (should-not (funcall user))
    (should-not (funcall modu)))

  (should-not (get-buffer "basic.eld"))
  (should-not erc-d-u--canned-buffers))

;; This indirectly tests `erc-d-u--canned-read' cleanup/teardown

(ert-deftest erc-d-u--rewrite-for-slow-mo ()
  (should-not (get-buffer "basic.eld"))
  (should-not (get-buffer "basic.eld<2>"))
  (should-not (get-buffer "basic.eld<3>"))
  (should-not erc-d-u--canned-buffers)
  (let ((exes (erc-d-u--canned-load-dialog 'basic))
        (exes-lower (erc-d-u--canned-load-dialog 'basic))
        (exes-custom (erc-d-u--canned-load-dialog 'basic))
        (reap (lambda (e) (cl-loop with p = (erc-d-u--read-dialog e)
                                   for s = (erc-d-u--read-exchange p)
                                   while s collect s))))
    (should (get-buffer "basic.eld"))
    (should (get-buffer "basic.eld<2>"))
    (should (get-buffer "basic.eld<3>"))
    (should (equal (list (get-buffer "basic.eld<3>")
                         (get-buffer "basic.eld<2>")
                         (get-buffer "basic.eld"))
                   erc-d-u--canned-buffers))

    (ert-info ("Rewrite for slowmo basic")
      (setq exes (erc-d-u--rewrite-for-slow-mo 10 exes))
      (should (equal (funcall reap exes)
                     '((pass 20.0 "PASS " (? ?:) "changeme"))))
      (should (equal (funcall reap exes)
                     '((nick 10.2 "NICK tester"))))
      (let ((r (funcall reap exes)))
        (should (equal (car r) '(user 10.2 "USER user 0 * :tester")))
        (should (equal
                 (car (last r))
                 '(0 ":irc.example.org 422 tester :MOTD File is missing"))))
      (should (equal (car (funcall reap exes))
                     '(mode-user 15 "MODE tester +i")))
      (should (equal (car (funcall reap exes))
                     '(mode-chan 13.2 "MODE #chan")))
      (should-not (erc-d-u--read-dialog exes)))

    (ert-info ("Rewrite for slowmo bounded")
      (setq exes-lower (erc-d-u--rewrite-for-slow-mo -5 exes-lower))
      (should (equal (funcall reap exes-lower)
                     '((pass 10.0 "PASS " (? ?:) "changeme"))))
      (should (equal (funcall reap exes-lower)
                     '((nick 5 "NICK tester"))))
      (should (equal (car (funcall reap exes-lower))
                     '(user 5 "USER user 0 * :tester")))
      (should (equal (car (funcall reap exes-lower))
                     '(mode-user 5 "MODE tester +i")))
      (should (equal (car (funcall reap exes-lower))
                     '(mode-chan 5 "MODE #chan")))
      (should-not (erc-d-u--read-dialog exes-lower)))

    (ert-info ("Rewrite for slowmo custom")
      (setq exes-custom (erc-d-u--rewrite-for-slow-mo
                         (lambda (n) (* 2 n)) exes-custom))
      (should (equal (funcall reap exes-custom)
                     '((pass 20.0 "PASS " (? ?:) "changeme"))))
      (should (equal (funcall reap exes-custom)
                     '((nick 0.4 "NICK tester"))))
      (should (equal (car (funcall reap exes-custom))
                     '(user 0.4 "USER user 0 * :tester")))
      (should (equal (car (funcall reap exes-custom))
                     '(mode-user 10 "MODE tester +i")))
      (should (equal (car (funcall reap exes-custom))
                     '(mode-chan 6.4 "MODE #chan")))
      (should-not (erc-d-u--read-dialog exes-custom))))

  (should-not (get-buffer "basic.eld"))
  (should-not (get-buffer "basic.eld<2>"))
  (should-not (get-buffer "basic.eld<3>"))
  (should-not erc-d-u--canned-buffers))

(ert-deftest erc-d--active-ex-p ()
  (let ((ring (make-ring 5)))
    (ert-info ("Empty ring returns nil for not active")
      (should-not (erc-d--active-ex-p ring)))
    (ert-info ("One fuzzy member returns nil for not active")
      (ring-insert ring (make-erc-d-exchange :tag '~foo))
      (should-not (erc-d--active-ex-p ring)))
    (ert-info ("One active member returns t for active")
      (ring-insert-at-beginning ring (make-erc-d-exchange :tag 'bar))
      (should (erc-d--active-ex-p ring)))))

(defun erc-d-tests--parse-message-upstream (raw)
  "Hack shim for parsing RAW line recvd from peer."
  (cl-letf (((symbol-function #'erc-handle-parsed-server-response)
             (lambda (_ p) p)))
    (let ((erc-active-buffer nil))
      (erc-parse-server-response nil raw))))

(ert-deftest erc-d-i--validate-tags ()
  (should (erc-d-i--validate-tags
           (concat "batch=4cc99692bf24a4bec4aa03da437364f5;"
                   "time=2021-01-04T00:32:13.839Z")))
  (should (erc-d-i--validate-tags "+foo=bar;baz=spam"))
  (should (erc-d-i--validate-tags "foo=\\:ok;baz=\\s"))
  (should (erc-d-i--validate-tags "foo=\303\247edilla"))
  (should (erc-d-i--validate-tags "foo=\\"))
  (should (erc-d-i--validate-tags "foo=bar\\baz"))
  (should-error (erc-d-i--validate-tags "foo=\\\\;baz=\\\r\\\n"))
  (should-error (erc-d-i--validate-tags "foo=\n"))
  (should-error (erc-d-i--validate-tags "foo=\0ok"))
  (should-error (erc-d-i--validate-tags "foo=bar baz"))
  (should-error (erc-d-i--validate-tags "foo=bar\r"))
  (should-error (erc-d-i--validate-tags "foo=bar;")))

(ert-deftest erc-d-i--parse-message ()
  (let* ((raw (concat "@time=2020-11-23T09:10:33.088Z "
                      ":tilde.chat BATCH +1 chathistory :#meta"))
         (upstream (erc-d-tests--parse-message-upstream raw))
         (ours (erc-d-i--parse-message raw)))

    (ert-info ("Baseline upstream")
      (should (equal (erc-response.unparsed upstream) raw))
      (should (equal (erc-response.sender upstream) "tilde.chat"))
      (should (equal (erc-response.command upstream) "BATCH"))
      (should (equal (erc-response.command-args upstream)
                     '("+1" "chathistory" "#meta")))
      (should (equal (erc-response.contents upstream) "#meta")))

    (ert-info ("Ours my not compare cl-equalp but is otherwise the same")
      (should (equal (erc-d-i-message.unparsed ours) raw))
      (should (equal (erc-d-i-message.sender ours) "tilde.chat"))
      (should (equal (erc-d-i-message.command ours) "BATCH"))
      (should (equal (erc-d-i-message.command-args ours)
                     '("+1" "chathistory" "#meta")))
      (should (equal (erc-d-i-message.contents ours) "#meta"))
      (should (equal (erc-d-i-message.tags ours)
                     '((time . "2020-11-23T09:10:33.088Z")))))

    (ert-info ("No compat decodes the whole message as utf-8")
      (setq ours (erc-d-i--parse-message
                  "@foo=\303\247edilla TAGMSG #ch\303\240n"
                  'decode))
      (should-not (erc-d-i-message.compat ours))
      (should (equal (erc-d-i-message.command-args ours) '("#chàn")))
      (should (equal (erc-d-i-message.contents ours) "#chàn"))
      (should (equal (erc-d-i-message.tags ours) '((foo . "çedilla")))))))

(ert-deftest erc-d-i--parse-message/privmsg ()
  (dolist (raw '(":Bob!~bob@gnu.org PRIVMSG #chan :one two"
                 ":Bob!~bob@gnu.org PRIVMSG #chan one"
                 ":Bob!~bob@gnu.org PRIVMSG #chan : "
                 ":Bob!~bob@gnu.org PRIVMSG #chan :"
                 "@account=bob :Bob!~bob@gnu.org PRIVMSG #chan one"
                 "@foo=bar;baz :Bob!~bob@gnu.org PRIVMSG #chan :one"))
    (dolist (slot '(unparsed
                    sender
                    command
                    command-args
                    contents
                    tags))
      (let ((ours (erc-d-i--parse-message raw))
            (orig (erc-d-tests--parse-message-upstream raw)))
        (ert-info ((format "slot: `%s', orig: %S, ours: %S"
                           slot orig ours))
          (if (eq slot 'tags)
              (should (equal (erc-response.tags orig)
                             (mapcar (pcase-lambda (`(,key . ,value))
                                       (if value
                                           (list (symbol-name key) value)
                                         (list (symbol-name key))))
                                     (reverse (erc-d-i-message.tags ours)))))
            (should
             (equal (cl-struct-slot-value 'erc-d-i-message slot ours)
                    (cl-struct-slot-value 'erc-response slot orig)))))))))

(ert-deftest erc-d-i--unescape-tag-value ()
  (should (equal (erc-d-i--unescape-tag-value
                  "\\sabc\\sdef\\s\\sxyz\\s")
                 " abc def  xyz "))
  (should (equal (erc-d-i--unescape-tag-value
                  "\\\\abc\\\\def\\\\\\\\xyz\\\\")
                 "\\abc\\def\\\\xyz\\"))
  (should (equal (erc-d-i--unescape-tag-value "a\\bc") "abc"))
  (should (equal (erc-d-i--unescape-tag-value
                  "\\\\abc\\\\def\\\\\\\\xyz\\")
                 "\\abc\\def\\\\xyz"))
  (should (equal (erc-d-i--unescape-tag-value "a\\:b\\r\\nc\\sd")
                 "a;b\r\nc d")))

(ert-deftest erc-d-i--escape-tag-value ()
  (should (equal (erc-d-i--escape-tag-value " abc def  xyz ")
                 "\\sabc\\sdef\\s\\sxyz\\s"))
  (should (equal (erc-d-i--escape-tag-value "\\abc\\def\\\\xyz\\")
                 "\\\\abc\\\\def\\\\\\\\xyz\\\\"))
  (should (equal (erc-d-i--escape-tag-value "a;b\r\nc d")
                 "a\\:b\\r\\nc\\sd")))

;; TODO add tests for msg-join, mask-match, userhost-split,
;; validate-hostname

(ert-deftest erc-d-i--parse-message--irc-parser-tests ()
  (let* ((data (with-temp-buffer
                 (insert-file-contents
                  (expand-file-name "irc-parser-tests.eld"
                                    (ert-resource-directory)))
                 (read (current-buffer))))
         (tests (assoc-default 'tests (assoc-default 'msg-split data)))
         input atoms m ours)
    (dolist (test tests)
      (setq input (assoc-default 'input test)
            atoms (assoc-default 'atoms test)
            m (erc-d-i--parse-message input))
      (ert-info ("Parses tags correctly")
        (setq ours (erc-d-i-message.tags m))
        (if-let* ((tags (assoc-default 'tags atoms)))
            (pcase-dolist (`(,key . ,value) ours)
              (should (string= (cdr (assq key tags)) (or value ""))))
          (should-not ours)))
      (ert-info ("Parses verbs correctly")
        (setq ours (erc-d-i-message.command m))
        (if-let* ((verbs (assoc-default 'verb atoms)))
            (should (string= (downcase verbs) (downcase ours)))
          (should (string-empty-p ours))))
      (ert-info ("Parses sources correctly")
        (setq ours (erc-d-i-message.sender m))
        (if-let* ((source (assoc-default 'source atoms)))
            (should (string= source ours))
          (should (string-empty-p ours))))
      (ert-info ("Parses params correctly")
        (setq ours (erc-d-i-message.command-args m))
        (if-let* ((params (assoc-default 'params atoms)))
            (should (equal ours params))
          (should-not ours))))))

(defun erc-d-tests--new-ex (existing raw-hunk)
  (let* ((f (lambda (_) (pop raw-hunk)))
         (sd (make-erc-d-u-scan-d :f f)))
    (setf (erc-d-exchange-hunk existing) (make-erc-d-u-scan-e :sd sd)
          (erc-d-exchange-spec existing) (make-erc-d-spec)))
  (erc-d--iter existing))

(ert-deftest erc-d--render-entries ()
  (let* ((erc-nick "foo")
         (dialog (make-erc-d-dialog :vars `((:a . 1)
                                            (c . ((a b) (: a space b)))
                                            (d . (c alpha digit))
                                            (bee . 2)
                                            (f . ,(lambda () "3"))
                                            (i . erc-nick))))
         (exchange (make-erc-d-exchange :dialog dialog))
         (mex (apply-partially #'erc-d-tests--new-ex exchange))
         it)

    (erc-d-exchange-reload dialog exchange)

    (ert-info ("Baseline Outgoing")
      (setq it (funcall mex '((0 "abc"))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "abc")))

    (ert-info ("Incoming are regexp escaped")
      (setq it (funcall mex '((i 0.0 "fsf" ".org"))))
      (should (equal (cons (funcall it) (funcall it)) '(i . 0.0)))
      (should (equal (funcall it) "\\`fsf\\.org")))

    (ert-info ("Incoming can access vars via rx-let")
      (setq it (funcall mex '((i 0.0 bee))))
      (should (equal (cons (funcall it) (funcall it)) '(i . 0.0)))
      (should (equal (funcall it) "\\`\002")))

    (ert-info ("Incoming rx-let params")
      (setq it (funcall mex '((i 0.0 d))))
      (should (equal (cons (funcall it) (funcall it)) '(i . 0.0)))
      (should (equal (funcall it) "\\`[[:alpha:]][[:space:]][[:digit:]]")))

    (ert-info ("Incoming literal rx forms")
      (setq it (funcall mex '((i 0.0 (= 3 alpha) ".org"))))
      (should (equal (cons (funcall it) (funcall it)) '(i . 0.0)))
      (should (equal (funcall it) "\\`[[:alpha:]]\\{3\\}\\.org")))

    (ert-info ("Self-quoting disallowed")
      (setq it (funcall mex '((0 :a "abc"))))
      (should (equal (funcall it) 0))
      (should-error (funcall it)))

    (ert-info ("Global vars and short vars")
      (setq it (funcall mex '((0 i f erc-nick))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "foo3foo")))

    (ert-info ("Exits clean")
      (should-not (funcall it))
      (should (equal (erc-d-dialog-vars dialog)
                     `((:a . 1)
                       (c . ((a b) (: a space b)))
                       (d . (c alpha digit))
                       (bee . 2)
                       (f . ,(alist-get 'f (erc-d-dialog-vars dialog)))
                       (i . erc-nick)))))))

(ert-deftest erc-d--render-entries--matches ()
  (let* ((alist (list
                 (cons 'f (lambda (a) (funcall a :match 1)))
                 (cons 'g (lambda () (match-string 2 "foo bar baz")))
                 (cons 'h (lambda (a) (concat (funcall a :match 0)
                                              (funcall a :request))))
                 (cons 'i (lambda (_ e) (erc-d-exchange-request e)))
                 (cons 'j (lambda ()
                            (set-match-data '(0 1))
                            (match-string 0 "j")))))
         (dialog (make-erc-d-dialog :vars alist))
         (exchange (make-erc-d-exchange :dialog dialog
                                        :request "foo bar baz"
                                        ;;            11  222
                                        :match-data '(4 11 4 6 8 11)))
         (mex (apply-partially #'erc-d-tests--new-ex exchange))
         it)

    (erc-d-exchange-reload dialog exchange)

    (ert-info ("One arg, match")
      (setq it (funcall mex '((0 f))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "ba")))

    (ert-info ("No args")
      (setq it (funcall mex '((0 g))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "baz")))

    (ert-info ("Second arg is exchange object")
      (setq it (funcall mex '((0 i))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "foo bar baz")))

    (ert-info ("One arg, multiple calls")
      (setq it (funcall mex '((0 h))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "bar bazfoo bar baz")))

    (ert-info ("Match data restored")
      (setq it (funcall mex '((0 j))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "j"))

      (setq it (funcall mex '((0 g))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "baz")))

    (ert-info ("Bad signature")
      (let ((qlist (list 'f '(lambda (p q x) (ignore)))))
        (setf (erc-d-dialog-vars dialog) qlist)
        (should-error (erc-d-exchange-reload dialog exchange))))))

(ert-deftest erc-d--render-entries--dynamic ()
  (let* ((alist (list
                 (cons 'foo "foo")
                 (cons 'f (lambda (a) (funcall a :get-binding 'foo)))
                 (cons 'h (lambda (a) (upcase (funcall a :get-var 'foo))))
                 (cons 'g (lambda (a)
                            (funcall a :rebind 'g (funcall a :get-var 'f))
                            "bar"))
                 (cons 'j (lambda (a) (funcall a :set "123") "abc"))
                 (cons 'k (lambda () "abc"))))
         (dialog (make-erc-d-dialog :vars alist))
         (exchange (make-erc-d-exchange :dialog dialog))
         (mex (apply-partially #'erc-d-tests--new-ex exchange))
         it)

    (erc-d-exchange-reload dialog exchange)

    (ert-info ("Initial reference calls function")
      (setq it (funcall mex '((0 j) (0 j))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "abc")))

    (ert-info ("Subsequent reference expands to string")
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "123")))

    (ert-info ("Outside manipulation: initial reference calls function")
      (setq it (funcall mex '((0 k) (0 k))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "abc")))

    (ert-info ("Outside manipulation: subsequent reference expands to string")
      (erc-d-exchange-rebind dialog exchange 'k "123")
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "123")))

    (ert-info ("Swap one function for another")
      (setq it (funcall mex '((0 g) (0 g))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "bar"))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "foo")))

    (ert-info ("Bindings accessible inside functions")
      (setq it (funcall mex '((0 f h))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "fooFOO")))

    (ert-info ("Rebuild alist by sending flag")
      (setq it (funcall mex '((0 f) (1 f) (2 f) (i 3 f))))
      (should (equal (funcall it) 0))
      (should (equal (funcall it) "foo"))
      (erc-d-exchange-rebind dialog exchange 'f "bar")
      (should (equal (funcall it) 1))
      (should (equal (funcall it) "bar"))
      (setq alist (setf (alist-get 'f (erc-d-dialog-vars dialog))
                        (lambda nil "baz")))
      (should (eq (funcall it) 2))
      (should (equal (funcall it 'reload) "baz"))
      (setq alist (setf (alist-get 'f (erc-d-dialog-vars dialog)) "spam"))
      (should (eq (funcall it) 'i))
      (should (eq (funcall it 'reload) 3))
      (should (equal (funcall it) "\\`spam")))))

(ert-deftest erc-d-t-with-cleanup ()
  (should-not (get-buffer "*echo*"))
  (should-not (get-buffer "*foo*"))
  (should-not (get-buffer "*bar*"))
  (should-not (get-buffer "*baz*"))
  (erc-d-t-with-cleanup
      ((echo (start-process "echo" (get-buffer-create "*echo*") "sleep" "1"))
       (buffer-foo (get-buffer-create "*foo*"))
       (buffer-bar (get-buffer-create "*bar*"))
       (clean-up (list (intern (process-name echo)))) ; let*
       buffer-baz)
      (ert-info ("Clean Up")
        (should (equal clean-up '(ran echo)))
        (should (bufferp buffer-baz))
        (should (bufferp buffer-foo))
        (setq buffer-foo nil))
    (setq buffer-baz (get-buffer-create "*baz*"))
    (push 'ran clean-up))
  (ert-info ("Buffers and procs destroyed")
    (should-not (get-buffer "*echo*"))
    (should-not (get-buffer "*bar*"))
    (should-not (get-buffer "*baz*")))
  (ert-info ("Buffer foo spared")
    (should (get-buffer "*foo*"))
    (kill-buffer "*foo*")))

(ert-deftest erc-d-t-wait-for ()
  :tags '(:unstable)
  (let (v)
    (run-at-time 0.2 nil (lambda () (setq v t)))
    (should (erc-d-t-wait-for 0.4 "result becomes non-nil" v))
    (should-error (erc-d-t-wait-for 0.4 "result stays nil" (not v)))
    (setq v nil)
    (should-not (erc-d-t-wait-for -0.4 "inverted stays nil" v))
    (run-at-time 0.2 nil (lambda () (setq v t)))
    (setq v nil)
    (should-error (erc-d-t-wait-for -0.4 "inverted becomes non-nil" v))))

(defvar erc-d-tests-with-server-password "changeme")

;; Compromise between removing `autojoin' from `erc-modules' entirely
;; and allowing side effects to meddle excessively
(defvar erc-autojoin-channels-alist)

;; This is only meant to be used by tests in this file.
(cl-defmacro erc-d-tests-with-server ((dumb-server-var erc-server-buffer-var)
                                      dialog &rest body)
  "Create server for DIALOG and run BODY.
DIALOG may also be a list of dialogs.  ERC-SERVER-BUFFER-VAR and
DUMB-SERVER-VAR are bound accordingly in BODY."
  (declare (indent 2))
  (when (eq '_ dumb-server-var)
    (setq dumb-server-var (make-symbol "dumb-server-var")))
  (when (eq '_ erc-server-buffer-var)
    (setq erc-server-buffer-var (make-symbol "erc-server-buffer-var")))
  (if (listp dialog)
      (setq dialog (mapcar (lambda (f) (list 'quote f)) dialog))
    (setq dialog `((quote ,dialog))))
  `(let* (auth-source-do-cache
          (,dumb-server-var (erc-d-run "localhost" t ,@dialog))
          ,erc-server-buffer-var
          ;;
          (erc-server-flood-penalty 0.05)
          erc-autojoin-channels-alist
          erc-after-connect
          erc-server-auto-reconnect)
     (should-not erc-d--slow-mo)
     (with-current-buffer "*erc-d-server*" (erc-d-t-search-for 4 "Starting"))
     ;; Allow important messages through, even in -batch mode.
     (advice-add #'erc-handle-login :around #'erc-d-t-silence-around)
     (advice-add #'erc-server-connect :around #'erc-d-t-silence-around)
     (unless (or noninteractive erc-debug-irc-protocol)
       (erc-toggle-debug-irc-protocol))
     (setq ,erc-server-buffer-var
           (erc :server "localhost"
                :password erc-d-tests-with-server-password
                :port (process-contact ,dumb-server-var :service)
                :nick "tester"
                :full-name "tester"))
     (unwind-protect
         (progn
           ,@body
           (erc-d-t-wait-for 1 "dumb-server death"
             (not (process-live-p ,dumb-server-var))))
       (when (process-live-p erc-server-process)
         (delete-process erc-server-process))
       (advice-remove #'erc-handle-login #'erc-d-t-silence-around)
       (advice-remove #'erc-server-connect #'erc-d-t-silence-around)
       (when noninteractive
         (kill-buffer ,erc-server-buffer-var)
         (erc-d-t-kill-related-buffers)))))

(defmacro erc-d-tests-with-failure-spy (found func-syms &rest body)
  "Wrap functions with advice for inspecting errors caused by BODY.
Do this for functions whose names appear in FUNC-SYMS.  When running
advice code, add errors to list FOUND.  Note: the teardown finalizer is
not added by default.  Also, `erc-d-linger-secs' likely has to be
nonzero for this to work."
  (declare (indent 2))
  ;; Catch errors thrown by timers that `should-error'ignores
  `(progn
     (let ((ad (lambda (f o &rest r)
                 (condition-case err
                     (apply o r)
                   (error (push err ,found)
                          (advice-remove f 'spy))))))
       (dolist (sym ,func-syms)
         (advice-add sym :around (apply-partially ad sym) '((name . spy)))))
     (progn ,@body)
     (dolist (sym ,func-syms)
       (advice-remove sym 'spy))
     (setq ,found (nreverse ,found))))

(ert-deftest erc-d-run-nonstandard-messages ()
  :tags '(:expensive-test)
  (let* ((erc-d-linger-secs 0.2)
         (dumb-server (erc-d-run "localhost" t 'nonstandard))
         (dumb-server-buffer (get-buffer "*erc-d-server*"))
         (expect (erc-d-t-make-expecter))
         client)
    (with-current-buffer "*erc-d-server*" (erc-d-t-search-for 4 "Starting"))
    (setq client (open-network-stream "erc-d-client" nil
                                      "localhost"
                                      (process-contact dumb-server :service)
                                      :coding 'binary))
    (ert-info ("Server splits CRLF delimited lines")
      (process-send-string client "ONE one\r\nTWO two\r\n")
      (with-current-buffer dumb-server-buffer
        (funcall expect 1 '(: "<- nonstandard:" (+ digit) " ONE one" eol))
        (funcall expect 1 '(regex "<- nonstandard:[[:digit:]]+ TWO two$"))))
    (ert-info ("Server doesn't discard empty lines")
      (process-send-string client "\r\n")
      (with-current-buffer dumb-server-buffer
        (funcall expect 1 '(regex "<- nonstandard:[[:digit:]]+ $"))))
    (ert-info ("Server preserves spaces")
      (process-send-string client " \r\n")
      (with-current-buffer dumb-server-buffer
        (funcall expect 1 '(regex "<- nonstandard:[[:digit:]]+ \\{2\\}$")))
      (process-send-string client "  \r\n")
      (with-current-buffer dumb-server-buffer
        (funcall expect 1 '(regex "<- nonstandard:[[:digit:]]+ \\{3\\}$"))))
    (erc-d-t-wait-for 3 "dumb-server death"
      (not (process-live-p dumb-server)))
    (delete-process client)
    (when noninteractive
      (kill-buffer dumb-server-buffer))))

(ert-deftest erc-d-run-basic ()
  :tags '(:expensive-test)
  (erc-d-tests-with-server (_ _) basic
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (erc-d-t-search-for 2 "hey"))
    (when noninteractive
      (kill-buffer "#chan"))))

(ert-deftest erc-d-run-eof ()
  :tags '(:expensive-test)
  (skip-unless noninteractive)
  (erc-d-tests-with-server (_ erc-s-buf) eof
    (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "#chan"))
      (erc-d-t-search-for 2 "hey"))
    (with-current-buffer erc-s-buf
      (process-send-eof erc-server-process))))

(ert-deftest erc-d-run-eof-fail ()
  :tags '(:expensive-test)
  (let (errors)
    (erc-d-tests-with-failure-spy errors '(erc-d--teardown)
      (erc-d-tests-with-server (_ _) eof
        (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
          (erc-d-t-search-for 2 "hey"))
        (erc-d-t-wait-for 10 errors)))
    (should (string-match-p "Timed out awaiting request.*__EOF__"
                            (cadr (pop errors))))))

(ert-deftest erc-d-run-linger ()
  :tags '(:unstable :expensive-test)
  (erc-d-tests-with-server (dumb-s _) linger
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (erc-d-t-search-for 2 "hey"))
    (with-current-buffer (process-buffer dumb-s)
      (erc-d-t-search-for 2 "Lingering for 1.00 seconds"))
    (with-current-buffer (process-buffer dumb-s)
      (erc-d-t-search-for 3 "Lingered for 1.00 seconds"))))

(ert-deftest erc-d-run-linger-fail ()
  :tags '(:unstable :expensive-test)
  (let ((erc-server-flood-penalty 0.1)
        errors)
    (erc-d-tests-with-failure-spy
        errors '(erc-d--teardown erc-d-command)
      (erc-d-tests-with-server (_ _) linger
        (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
          (erc-d-t-search-for 2 "hey")
          (erc-cmd-MSG "#chan hi"))
        (erc-d-t-wait-for 10 "Bad match" errors)))
    (should (string-match-p "Match failed.*hi" (cadr (pop errors))))))

(ert-deftest erc-d-run-linger-direct ()
  :tags '(:unstable :expensive-test)
  (let* ((dumb-server (erc-d-run "localhost" t
                                 'linger-multi-a 'linger-multi-b))
         (port (process-contact dumb-server :service))
         (dumb-server-buffer (get-buffer "*erc-d-server*"))
         (client-buffer-a (get-buffer-create "*erc-d-client-a*"))
         (client-buffer-b (get-buffer-create "*erc-d-client-b*"))
         (start (current-time))
         client-a client-b)
    (with-current-buffer "*erc-d-server*" (erc-d-t-search-for 4 "Starting"))
    (setq client-a (open-network-stream "erc-d-client-a" client-buffer-a
                                        "localhost" port
                                        :coding 'binary)
          client-b (open-network-stream "erc-d-client-b" client-buffer-b
                                        "localhost" port
                                        :coding 'binary))
    (process-send-string client-a "PASS :a\r\n")
    (sleep-for 0.01)
    (process-send-string client-b "PASS :b\r\n")
    (sleep-for 0.01)
    (erc-d-t-wait-for 3 "dumb-server death"
      (not (process-live-p dumb-server)))
    (ert-info ("Ensure linger of one second")
      (should (time-less-p 1 (time-subtract (current-time) start)))
      (should (time-less-p (time-subtract (current-time) start) 1.5)))
    (delete-process client-a)
    (delete-process client-b)
    (when noninteractive
      (kill-buffer client-buffer-a)
      (kill-buffer client-buffer-b)
      (kill-buffer dumb-server-buffer))))

(ert-deftest erc-d-run-drop-direct ()
  :tags '(:unstable)
  (let* ((dumb-server (erc-d-run "localhost" t 'drop-a 'drop-b))
         (port (process-contact dumb-server :service))
         (dumb-server-buffer (get-buffer "*erc-d-server*"))
         (client-buffer-a (get-buffer-create "*erc-d-client-a*"))
         (client-buffer-b (get-buffer-create "*erc-d-client-b*"))
         (start (current-time))
         client-a client-b)
    (with-current-buffer "*erc-d-server*" (erc-d-t-search-for 4 "Starting"))
    (setq client-a (open-network-stream "erc-d-client-a" client-buffer-a
                                        "localhost" port
                                        :coding 'binary)
          client-b (open-network-stream "erc-d-client-b" client-buffer-b
                                        "localhost" port
                                        :coding 'binary))
    (process-send-string client-a "PASS :a\r\n")
    (sleep-for 0.01)
    (process-send-string client-b "PASS :b\r\n")
    (erc-d-t-wait-for 3 "client-a dies" (not (process-live-p client-a)))
    (should (time-less-p (time-subtract (current-time) start) 0.32))
    (erc-d-t-wait-for 3 "dumb-server death"
      (not (process-live-p dumb-server)))
    (ert-info ("Ensure linger of one second")
      (should (time-less-p 1 (time-subtract (current-time) start))))
    (delete-process client-a)
    (delete-process client-b)
    (when noninteractive
      (kill-buffer client-buffer-a)
      (kill-buffer client-buffer-b)
      (kill-buffer dumb-server-buffer))))

(ert-deftest erc-d-run-no-match ()
  :tags '(:expensive-test)
  (let ((erc-d-linger-secs 1)
        erc-server-auto-reconnect
        errors)
    (erc-d-tests-with-failure-spy errors '(erc-d--teardown erc-d-command)
      (erc-d-tests-with-server (_ erc-server-buffer) no-match
        (with-current-buffer erc-server-buffer
          (erc-d-t-search-for 2 "away")
          (erc-cmd-JOIN "#foo")
          (erc-d-t-wait-for 10 "Bad match" errors))))
    (should (string-match-p "Match failed.*foo.*chan" (cadr (pop errors))))
    (should-not (get-buffer "#foo"))))

(ert-deftest erc-d-run-timeout ()
  :tags '(:expensive-test)
  (let ((erc-d-linger-secs 1)
        err errors)
    (erc-d-tests-with-failure-spy errors '(erc-d--teardown)
      (erc-d-tests-with-server (_ _) timeout
        (erc-d-t-wait-for 10 "error caught" errors)))
    (setq err (pop errors))
    (should (eq (car err) 'erc-d-timeout))
    (should (string-match-p "Timed out" (cadr err)))))

(ert-deftest erc-d-run-unexpected ()
  :tags '(:expensive-test)
  (let ((erc-d-linger-secs 2)
        errors)
    (erc-d-tests-with-failure-spy
        errors '(erc-d--teardown erc-d-command)
      (erc-d-tests-with-server (_ _) unexpected
        (ert-info ("All specs consumed when more input arrives")
          (erc-d-t-wait-for 10 "error caught" (cdr errors)))))
    (should (string-match-p "unexpected.*MODE" (cadr (pop errors))))
    ;; Nonsensical normally because func would have already exited when
    ;; first error was thrown
    (should (string-match-p "Match failed" (cadr (pop errors))))))

(ert-deftest erc-d-run-unexpected-depleted ()
  :tags '(:expensive-test)
  (let ((erc-d-linger-secs 3)
        errors)
    (erc-d-tests-with-failure-spy errors '(erc-d--teardown erc-d-command)
      (let* ((dumb-server-buffer (get-buffer-create "*erc-d-server*"))
             (dumb-server (erc-d-run "localhost" t 'depleted))
             (expect (erc-d-t-make-expecter))
             (client-buf (get-buffer-create "*erc-d-client*"))
             client-proc)
        (with-current-buffer dumb-server-buffer
          (erc-d-t-search-for 3 "Starting"))
        (setq client-proc (make-network-process
                           :buffer client-buf
                           :name "erc-d-client"
                           :family 'ipv4
                           :noquery t
                           :coding 'binary
                           :service (process-contact dumb-server :service)
                           :host "localhost"))
        (with-current-buffer dumb-server-buffer
          (funcall expect 3 "open from"))
        (process-send-string client-proc "PASS :changeme\r\n")
        (sleep-for 0.01)
        (process-send-string client-proc "NICK tester\r\n")
        (sleep-for 0.01)
        (process-send-string client-proc "USER user 0 * :tester\r\n")
        (sleep-for 0.01)
        (when (process-live-p client-proc)
          (process-send-string client-proc "BLAH :too much\r\n")
          (sleep-for 0.01))
        (with-current-buffer client-buf
          (funcall expect 3 "Welcome to the Internet"))
        (erc-d-t-wait-for 2 "dumb-server death"
          (not (process-live-p dumb-server)))
        (delete-process client-proc)
        (when noninteractive
          (kill-buffer client-buf)
          (kill-buffer dumb-server-buffer))))
    (should (string-match-p "unexpected.*BLAH" (cadr (pop errors))))
    ;; Wouldn't happen IRL
    (should (string-match-p "unexpected.*BLAH" (cadr (pop errors))))
    (should-not errors)))

(defun erc-d-tests--dynamic-match-user (_dialog exchange)
  "Shared pattern/response handler for canned dynamic DIALOG test."
  (should (string= (match-string 1 (erc-d-exchange-request exchange))
                   "tester")))

(defun erc-d-tests--run-dynamic ()
  "Perform common assertions for \"dynamic\" dialog."
  (erc-d-tests-with-server (dumb-server erc-server-buffer) dynamic
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (erc-d-t-search-for 2 "tester: hey"))
    (with-current-buffer erc-server-buffer
      (let ((expect (erc-d-t-make-expecter)))
        (funcall expect 2 "host is irc.fsf.org")
        (funcall expect 2 "modes for tester")))
    (with-current-buffer (process-buffer dumb-server)
      (erc-d-t-search-for 2 "irc.fsf.org"))
    (when noninteractive
      (kill-buffer "#chan"))))

(ert-deftest erc-d-run-dynamic-default-match ()
  :tags '(:expensive-test)
  (let* (dynamic-tally
         (erc-d-tmpl-vars '((user . "user")
                            (ignored . ((a b) (: a space b)))
                            (realname . (group (+ graph)))))
         (nick (lambda (a)
                 (push '(nick . match-user) dynamic-tally)
                 (funcall a :set (funcall a :match 1) 'export)))
         (dom (lambda (a)
                (push '(dom . match-user) dynamic-tally)
                (funcall a :set erc-d-server-fqdn)))
         (erc-d-match-handlers
          (list :user (lambda (d e)
                        (erc-d-exchange-rebind d e 'nick nick)
                        (erc-d-exchange-rebind d e 'dom dom)
                        (erc-d-tests--dynamic-match-user d e))
                :mode-user (lambda (d e)
                             (erc-d-exchange-rebind d e 'nick "tester")
                             (erc-d-exchange-rebind d e 'dom dom))))
         (erc-d-server-fqdn "irc.fsf.org"))
    (erc-d-tests--run-dynamic)
    (should (equal '((dom . match-user) (nick . match-user) (dom . match-user))
                   dynamic-tally))))

(ert-deftest erc-d-run-dynamic-default-match-rebind ()
  :tags '(:expensive-test)
  (let* (tally
         ;;
         (erc-d-tmpl-vars '((user . "user")
                            (ignored . ((a b) (: a space b)))
                            (realname . (group (+ graph)))))
         (erc-d-match-handlers
          (list :user
                (lambda (d e)
                  (erc-d-exchange-rebind
                   d e 'nick
                   (lambda (a)
                     (push 'bind-nick tally)
                     (funcall a :rebind 'nick (funcall a :match 1) 'export)))
                  (erc-d-exchange-rebind
                   d e 'dom
                   (lambda ()
                     (push 'bind-dom tally)
                     (erc-d-exchange-rebind d e 'dom erc-d-server-fqdn)))
                  (erc-d-tests--dynamic-match-user d e))
                :mode-user
                (lambda (d e)
                  (erc-d-exchange-rebind d e 'nick "tester")
                  (erc-d-exchange-rebind d e 'dom erc-d-server-fqdn))))
         (erc-d-server-fqdn "irc.fsf.org"))
    (erc-d-tests--run-dynamic)
    (should (equal '(bind-nick bind-dom) tally))))

(ert-deftest erc-d-run-dynamic-runtime-stub ()
  :tags '(:expensive-test)
  (let ((erc-d-tmpl-vars '((token . (group (or "barnet" "foonet")))))
        (erc-d-match-handlers
         (list :pass (lambda (d _e)
                       (erc-d-load-replacement-dialog d 'dynamic-foonet))))
        (erc-d-tests-with-server-password "foonet:changeme"))
    (erc-d-tests-with-server (_ erc-server-buffer)
        (dynamic-stub dynamic-foonet)
      (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "#chan"))
        (erc-d-t-search-for 2 "alice:")
        (erc-d-t-absent-for 0.1 "joe"))
      (with-current-buffer erc-server-buffer
        (let ((expect (erc-d-t-make-expecter)))
          (funcall expect 2 "host is irc.foonet.org")
          (funcall expect 2 "NETWORK=FooNet")))
      (when noninteractive
        (kill-buffer "#chan")))))

(ert-deftest erc-d-run-dynamic-runtime-stub-skip ()
  :tags '(:expensive-test)
  (let ((erc-d-tmpl-vars '((token . "barnet")))
        (erc-d-match-handlers
         (list :pass (lambda (d _e)
                       (erc-d-load-replacement-dialog
                        d 'dynamic-barnet 1))))
        (erc-d-tests-with-server-password "barnet:changeme"))
    (erc-d-tests-with-server (_ erc-server-buffer)
        (dynamic-stub dynamic-barnet)
      (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "#chan"))
        (erc-d-t-search-for 2 "joe:")
        (erc-d-t-absent-for 0.1 "alice"))
      (with-current-buffer erc-server-buffer
        (let ((expect (erc-d-t-make-expecter)))
          (funcall expect 2 "host is irc.barnet.org")
          (funcall expect 2 "NETWORK=BarNet")))
      (when noninteractive
        (kill-buffer "#chan")))))

;; Two servers, in-process, one client per
(ert-deftest erc-d-run-dual-direct ()
  :tags '(:expensive-test)
  (let* ((erc-d--slow-mo -1)
         (server-a (erc-d-run "localhost" t "erc-d-server-a" 'dynamic-foonet))
         (server-b (erc-d-run "localhost" t "erc-d-server-b" 'dynamic-barnet))
         (server-a-buffer (get-buffer "*erc-d-server-a*"))
         (server-b-buffer (get-buffer "*erc-d-server-b*"))
         (client-a-buffer (get-buffer-create "*erc-d-client-a*"))
         (client-b-buffer (get-buffer-create "*erc-d-client-b*"))
         client-a client-b)
    (with-current-buffer server-a-buffer (erc-d-t-search-for 4 "Starting"))
    (with-current-buffer server-b-buffer (erc-d-t-search-for 4 "Starting"))
    (setq client-a (make-network-process
                    :buffer client-a-buffer
                    :name "erc-d-client-a"
                    :family 'ipv4
                    :noquery t
                    :coding 'binary
                    :service (process-contact server-a :service)
                    :host "localhost")
          client-b (make-network-process
                    :buffer client-b-buffer
                    :name "erc-d-client-b"
                    :family 'ipv4
                    :noquery t
                    :coding 'binary
                    :service (process-contact server-b :service)
                    :host "localhost"))
    ;; Also tests slo-mo indirectly because FAKE would fail without it
    (process-send-string client-a "NICK tester\r\n")
    (process-send-string client-b "FAKE noop\r\nNICK tester\r\n")
    (sleep-for 0.01)
    (process-send-string client-a "USER user 0 * :tester\r\n")
    (process-send-string client-b "USER user 0 * :tester\r\n")
    (sleep-for 0.01)
    (process-send-string client-a "MODE tester +i\r\n")
    (process-send-string client-b "MODE tester +i\r\n")
    (sleep-for 0.01)
    (process-send-string client-a "MODE #chan\r\n")
    (process-send-string client-b "MODE #chan\r\n")
    (sleep-for 0.01)
    (erc-d-t-wait-for 2 "server-a death" (not (process-live-p server-a)))
    (erc-d-t-wait-for 2 "server-b death" (not (process-live-p server-b)))
    (when noninteractive
      (kill-buffer client-a-buffer)
      (kill-buffer client-b-buffer)
      (kill-buffer server-a-buffer)
      (kill-buffer server-b-buffer))))

;; This can be removed; only exists to get a baseline for next test
(ert-deftest erc-d-run-fuzzy-direct ()
  :tags '(:expensive-test)
  (let* ((erc-d-tmpl-vars
          `((now . ,(lambda () (format-time-string "%FT%T.%3NZ" nil t)))))
         (dumb-server (erc-d-run "localhost" t 'fuzzy))
         (dumb-server-buffer (get-buffer "*erc-d-server*"))
         (client-buffer (get-buffer-create "*erc-d-client*"))
         client)
    (with-current-buffer "*erc-d-server*" (erc-d-t-search-for 4 "Starting"))
    (setq client (make-network-process
                  :buffer client-buffer
                  :name "erc-d-client"
                  :family 'ipv4
                  :noquery t
                  :coding 'binary
                  :service (process-contact dumb-server :service)
                  :host "localhost"))
    ;; We could also just send this as a single fatty
    (process-send-string client "PASS :changeme\r\n")
    (sleep-for 0.01)
    (process-send-string client "NICK tester\r\n")
    (sleep-for 0.01)
    (process-send-string client "USER user 0 * :tester\r\n")
    (sleep-for 0.01)
    (process-send-string client "MODE tester +i\r\n")
    (sleep-for 0.01)
    (process-send-string client "JOIN #bar\r\n")
    (sleep-for 0.01)
    (process-send-string client "JOIN #foo\r\n")
    (sleep-for 0.01)
    (process-send-string client "MODE #bar\r\n")
    (sleep-for 0.01)
    (process-send-string client "MODE #foo\r\n")
    (sleep-for 0.01)
    (erc-d-t-wait-for 1 "dumb-server death"
      (not (process-live-p dumb-server)))
    (when noninteractive
      (kill-buffer client-buffer)
      (kill-buffer dumb-server-buffer))))

;; Without adjusting penalty, takes ~15 secs. With is comparable to direct ^.
(ert-deftest erc-d-run-fuzzy ()
  :tags '(:expensive-test)
  (let ((erc-server-flood-penalty 1.2) ; penalty < margin/sends is basically 0
        (erc-d-linger-secs 0.1)
        (erc-d-tmpl-vars
         `((now . ,(lambda () (format-time-string "%FT%T.%3NZ" nil t)))))
        erc-server-auto-reconnect)
    (erc-d-tests-with-server (_ erc-server-buffer) fuzzy
      (with-current-buffer erc-server-buffer
        (erc-d-t-search-for 2 "away")
        (goto-char erc-input-marker)
        (erc-cmd-JOIN "#bar"))
      (erc-d-t-wait-for 2 (get-buffer "#bar"))
      (with-current-buffer erc-server-buffer
        (erc-cmd-JOIN "#foo"))
      (erc-d-t-wait-for 20 (get-buffer "#foo"))
      (with-current-buffer "#bar"
        (erc-d-t-search-for 1 "was created on"))
      (with-current-buffer "#foo"
        (erc-d-t-search-for 5 "was created on")))))

(ert-deftest erc-d-run-no-block ()
  :tags '(:expensive-test)
  (let ((erc-server-flood-penalty 1)
        (erc-d-linger-secs 1.2)
        (expect (erc-d-t-make-expecter))
        erc-server-auto-reconnect)
    (erc-d-tests-with-server (_ erc-server-buffer) no-block
      (with-current-buffer erc-server-buffer
        (funcall expect 2 "away")
        (funcall expect 1 erc-prompt)
        (with-current-buffer erc-server-buffer (erc-cmd-JOIN "#foo")))
      (with-current-buffer (erc-d-t-wait-for 2 (get-buffer "#foo"))
        (funcall expect 2 "was created on"))

      (ert-info ("Join #bar")
        (with-current-buffer erc-server-buffer (erc-cmd-JOIN "#bar"))
        (erc-d-t-wait-for 2 (get-buffer "#bar")))

      (with-current-buffer "#bar" (funcall expect 1 "was created on"))

      (ert-info ("Server expects next pattern but keeps sending")
        (with-current-buffer "#foo" (funcall expect 2 "Rosalind, I will "))
        (with-current-buffer "#bar" (funcall expect 1 "hi 123"))
        (with-current-buffer "#foo"
          (should-not (search-forward "<bob> I am heard" nil t))
          (funcall expect 1.5 "<bob> I am heard"))))))

(defun erc-d-tests--run-proxy-direct (dumb-server dumb-server-buffer port)
  "Start DUMB-SERVER with DUMB-SERVER-BUFFER and PORT.
These are steps shared by in-proc and subproc variants testing a
bouncer-like setup."
  (when (version< emacs-version "28") (ert-skip "TODO connection refused"))
  (let ((client-buffer-foo (get-buffer-create "*erc-d-client-foo*"))
        (client-buffer-bar (get-buffer-create "*erc-d-client-bar*"))
        (expect (erc-d-t-make-expecter))
        client-foo
        client-bar)
    (setq client-foo (make-network-process
                      :buffer client-buffer-foo
                      :name "erc-d-client-foo"
                      :family 'ipv4
                      :noquery t
                      :coding 'binary
                      :service port
                      :host "localhost")
          client-bar (make-network-process
                      :buffer client-buffer-bar
                      :name "erc-d-client-bar"
                      :family 'ipv4
                      :noquery t
                      :coding 'binary
                      :service port
                      :host "localhost"))
    (with-current-buffer dumb-server-buffer
      (funcall expect 3 "open from"))
    (process-send-string client-foo "PASS :foo:changeme\r\n")
    (process-send-string client-bar "PASS :bar:changeme\r\n")
    (sleep-for 0.01)
    (process-send-string client-foo "NICK tester\r\n")
    (process-send-string client-bar "NICK tester\r\n")
    (sleep-for 0.01)
    (process-send-string client-foo "USER user 0 * :tester\r\n")
    (process-send-string client-bar "USER user 0 * :tester\r\n")
    (sleep-for 0.01)
    (process-send-string client-foo "MODE tester +i\r\n")
    (process-send-string client-bar "MODE tester +i\r\n")
    (sleep-for 0.01)
    (with-current-buffer client-buffer-foo
      (funcall expect 3 "FooNet")
      (funcall expect 3 "irc.foo.net")
      (funcall expect 3 "marked as being away")
      (goto-char (point-min))
      (should-not (search-forward "bar" nil t)))
    (with-current-buffer client-buffer-bar
      (funcall expect 3 "BarNet")
      (funcall expect 3 "irc.bar.net")
      (funcall expect 3 "marked as being away")
      (goto-char (point-min))
      (should-not (search-forward "foo" nil t)))
    (erc-d-t-wait-for 2 "dumb-server death"
      (not (process-live-p dumb-server)))
    (delete-process client-foo)
    (delete-process client-bar)
    (when noninteractive
      (kill-buffer client-buffer-foo)
      (kill-buffer client-buffer-bar)
      (kill-buffer dumb-server-buffer))))

;; This test shows the simplest way to set up template variables: put
;; everything needed for the whole session in `erc-d-tmpl-vars' before
;; starting the server.

(ert-deftest erc-d-run-proxy-direct-spec-vars ()
  :tags '(:expensive-test)
  (let* ((dumb-server-buffer (get-buffer-create "*erc-d-server*"))
         (erc-d-linger-secs 0.5)
         (erc-d-tmpl-vars
          `((network . (group (+ alpha)))
            (fqdn . ,(lambda (a)
                       (let ((network (funcall a :match 1 'pass)))
                         (should (member network '("foo" "bar")))
                         (funcall a :set (concat "irc." network ".net")))))
            (net . ,(lambda (a)
                      (let ((network (funcall a :match 1 'pass)))
                        (should (member network '("foo" "bar")))
                        (concat (capitalize network) "Net"))))))
         (dumb-server (erc-d-run "localhost" t 'proxy-foonet 'proxy-barnet))
         (port (process-contact dumb-server :service)))
    (with-current-buffer dumb-server-buffer
      (erc-d-t-search-for 3 "Starting"))
    (erc-d-tests--run-proxy-direct dumb-server dumb-server-buffer port)))

(cl-defun erc-d-tests--start-server (&key dialogs buffer linger program libs)
  "Start and return a server in a subprocess using BUFFER and PORT.
DIALOGS are symbols representing the base names of dialog files in
`erc-d-u-canned-dialog-dir'.  LIBS are extra files to load."
  (push (locate-library "erc-d" nil (list erc-d-u--library-directory)) libs)
  (cl-assert (car libs))
  (let* ((args `("erc-d-server" ,buffer
                 ,(concat invocation-directory invocation-name)
                 "-Q" "-batch" "-L" ,erc-d-u--library-directory
                 ,@(let (o) (while libs (push (pop libs) o) (push "-l" o)) o)
                 "-eval" ,(format "%S" program) "-f" "erc-d-serve"
                 ,@(when linger (list "--linger" (number-to-string linger)))
                 ,@(mapcar #'erc-d-u--expand-dialog-symbol dialogs)))
         (proc (apply #'start-process args)))
    (set-process-query-on-exit-flag proc nil)
    (with-current-buffer buffer
      (erc-d-t-search-for 10 "Starting")
      (search-forward " (")
      (backward-char))
    (let ((pair (read buffer)))
      (cons proc (cdr pair)))))

(ert-deftest erc-d-run-proxy-direct-subprocess ()
  :tags '(:expensive-test)
  (let* ((buffer (get-buffer-create "*erc-d-server*"))
         ;; These are quoted because they're passed as printed forms to subproc
         (fqdn '(lambda (a e)
                  (let* ((d (erc-d-exchange-dialog e))
                         (name (erc-d-dialog-name d)))
                    (funcall a :set (if (eq name 'proxy-foonet)
                                        "irc.foo.net"
                                      "irc.bar.net")))))
         (net '(lambda (a)
                 (funcall a :rebind 'net
                          (if (eq (funcall a :dialog-name) 'proxy-foonet)
                              "FooNet"
                            "BarNet"))))
         (program `(setq erc-d-tmpl-vars '((fqdn . ,fqdn)
                                           (net . ,net)
                                           (network . (group (+ alpha))))))
         (port (erc-d-tests--start-server
                :linger 0.3
                :program program
                :buffer buffer
                :dialogs '(proxy-foonet proxy-barnet)))
         (server (pop port)))
    (erc-d-tests--run-proxy-direct server buffer port)))

(ert-deftest erc-d-run-proxy-direct-subprocess-lib ()
  :tags '(:expensive-test)
  (let* ((buffer (get-buffer-create "*erc-d-server*"))
         (lib (expand-file-name "proxy-subprocess.el"
                                (ert-resource-directory)))
         (port (erc-d-tests--start-server :linger 0.3
                                          :buffer buffer
                                          :dialogs '(proxy-foonet proxy-barnet)
                                          :libs (list lib)))
         (server (pop port)))
    (erc-d-tests--run-proxy-direct server buffer port)))

(ert-deftest erc-d-run-no-pong ()
  :tags '(:expensive-test)
  (let* (erc-d-auto-pong
         ;;
         (erc-d-tmpl-vars
          `((nonce . (group (: digit digit)))
            (echo . ,(lambda (a)
                       (should (string= (funcall a :match 1) "42")) "42"))))
         (dumb-server-buffer (get-buffer-create "*erc-d-server*"))
         (dumb-server (erc-d-run "localhost" t 'no-pong))
         (expect (erc-d-t-make-expecter))
         (client-buf (get-buffer-create "*erc-d-client*"))
         client-proc)
    (with-current-buffer dumb-server-buffer
      (erc-d-t-search-for 3 "Starting"))
    (setq client-proc (make-network-process
                       :buffer client-buf
                       :name "erc-d-client"
                       :family 'ipv4
                       :noquery t
                       :coding 'binary
                       :service (process-contact dumb-server :service)
                       :host "localhost"))
    (with-current-buffer dumb-server-buffer
      (funcall expect 3 "open from"))
    (process-send-string client-proc "PASS :changeme\r\nNICK tester\r\n")
    (sleep-for 0.01)
    (process-send-string client-proc "USER user 0 * :tester\r\n")
    (sleep-for 0.01)
    (process-send-string client-proc "MODE tester +i\r\n")
    (sleep-for 0.01)
    (with-current-buffer client-buf
      (funcall expect 3 "ExampleOrg")
      (funcall expect 3 "irc.example.org")
      (funcall expect 3 "marked as being away"))
    (ert-info ("PING is not intercepted by specialized method")
      (process-send-string client-proc "PING 42\r\n")
      (with-current-buffer client-buf
        (funcall expect 3 "PONG")))
    (erc-d-t-wait-for 2 "dumb-server death"
      (not (process-live-p dumb-server)))
    (delete-process client-proc)
    (when noninteractive
      (kill-buffer client-buf)
      (kill-buffer dumb-server-buffer))))

;; Inspect replies as they arrive within a single exchange, i.e., ensure we
;; don't regress to prior buggy version in which inspection wasn't possible
;; until all replies had been sent by the server.
(ert-deftest erc-d-run-incremental ()
  :tags '(:expensive-test)
  (let ((erc-server-flood-penalty 0)
        (expect (erc-d-t-make-expecter))
        erc-d-linger-secs)
    (erc-d-tests-with-server (_ erc-server-buffer) incremental
      (with-current-buffer erc-server-buffer
        (funcall expect 3 "marked as being away"))
      (with-current-buffer erc-server-buffer
        (erc-cmd-JOIN "#foo"))
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (funcall expect 1 "Users on #foo")
        (funcall expect 1 "Look for me")
        (not (search-forward "Done" nil t))
        (funcall expect 10 "Done")
        (erc-send-message "Hi")))))

(ert-deftest erc-d-unix-socket-direct ()
  :tags '(:expensive-test)
  (skip-unless (featurep 'make-network-process '(:family local)))
  (let* ((erc-d-linger-secs 0.1)
         (sock (expand-file-name "erc-d.sock" temporary-file-directory))
         (dumb-server (erc-d-run nil sock 'basic))
         (dumb-server-buffer (get-buffer "*erc-d-server*"))
         (client-buffer (get-buffer-create "*erc-d-client*"))
         client)
    (with-current-buffer "*erc-d-server*"
      (erc-d-t-search-for 4 "Starting"))
    (unwind-protect
        (progn
          (setq client (make-network-process
                        :buffer client-buffer
                        :name "erc-d-client"
                        :family 'local
                        :noquery t
                        :coding 'binary
                        :service sock))
          (process-send-string client "PASS :changeme\r\n")
          (sleep-for 0.01)
          (process-send-string client "NICK tester\r\n")
          (sleep-for 0.01)
          (process-send-string client "USER user 0 * :tester\r\n")
          (sleep-for 0.1)
          (process-send-string client "MODE tester +i\r\n")
          (sleep-for 0.01)
          (process-send-string client "MODE #chan\r\n")
          (sleep-for 0.01)
          (erc-d-t-wait-for 1 "dumb-server death"
            (not (process-live-p dumb-server)))
          (when noninteractive
            (kill-buffer client-buffer)
            (kill-buffer dumb-server-buffer)))
      (delete-file sock))))

(ert-deftest erc-d-run-direct-foreign-protocol ()
  :tags '(:expensive-test)
  (let* ((server (erc-d-run "localhost" t "erc-d-server" 'foreign
                            :ending "\n"))
         (server-buffer (get-buffer "*erc-d-server*"))
         (client-buffer (get-buffer-create "*erc-d-client*"))
         client)
    (with-current-buffer server-buffer (erc-d-t-search-for 4 "Starting"))
    (setq client (make-network-process
                  :buffer client-buffer
                  :name "erc-d-client"
                  :family 'ipv4
                  :noquery t
                  :coding 'binary
                  :service (process-contact server :service)
                  :host "localhost"))
    (process-send-string client "ONE one\n")
    (with-current-buffer client-buffer
      (erc-d-t-search-for 5 "echo ONE one"))
    (process-send-string client "TWO two\n")
    (with-current-buffer client-buffer
      (erc-d-t-search-for 2 "echo TWO two"))
    (erc-d-t-wait-for 2 "server death" (not (process-live-p server)))
    (when noninteractive
      (kill-buffer client-buffer)
      (kill-buffer server-buffer))))

;;; erc-d-tests.el ends here
