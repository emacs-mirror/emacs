;;; sgml-mode-tests.el --- Tests for sgml-mode

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Przemysław Wojnowski <esperanto@cumego.com>
;; Keywords: tests

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

(require 'sgml-mode)
(require 'ert)

(defmacro sgml-with-content (content &rest body)
  "Insert CONTENT into a temporary `sgml-mode' buffer and execute BODY on it.
The point is set to the beginning of the buffer."
  `(with-temp-buffer
     (sgml-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; sgml-delete-tag

(ert-deftest sgml-delete-tag-should-not-delete-tags-when-wrong-args ()
  "Don't delete tag, when number of tags to delete is not positive number."
  (let ((content "<p>Valar Morghulis</p>"))
    (sgml-with-content
     content
     (sgml-delete-tag -1)
     (should (string= content (buffer-string)))
     (sgml-delete-tag 0)
     (should (string= content (buffer-string))))))

(ert-deftest sgml-delete-tag-should-delete-tags-n-times ()
  ;; Delete only 1, when 1 available:
  (sgml-with-content
   "<br />"
   (sgml-delete-tag 1)
   (should (string= "" (buffer-string))))
  ;; Delete from position on whitespaces before tag:
  (sgml-with-content
   " \t\n<br />"
   (sgml-delete-tag 1)
   (should (string= "" (buffer-string))))
  ;; Delete from position on tag:
  (sgml-with-content
   "<br />"
   (goto-char 3)
   (sgml-delete-tag 1)
   (should (string= "" (buffer-string))))
  ;; Delete one by one:
  (sgml-with-content
   "<h1><p>You know nothing, Jon Snow.</p></h1>"
   (sgml-delete-tag 1)
   (should (string= "<p>You know nothing, Jon Snow.</p>" (buffer-string)))
   (sgml-delete-tag 1)
   (should (string= "You know nothing, Jon Snow." (buffer-string))))
  ;; Delete 2 at a time, when 2 available:
  (sgml-with-content
   "<h1><p>You know nothing, Jon Snow.</p></h1>"
   (sgml-delete-tag 2)
   (should (string= "You know nothing, Jon Snow." (buffer-string)))))

(ert-deftest sgml-delete-tag-should-delete-unclosed-tag ()
  (sgml-with-content
   "<ul><li>Keep your stones connected.</ul>"
   (goto-char 5)                   ; position on "li" tag
   (sgml-delete-tag 1)
   (should (string= "<ul>Keep your stones connected.</ul>" (buffer-string)))))

(ert-deftest sgml-delete-tag-should-signal-error-for-malformed-tags ()
  (let ((content "<h1><h2>Drakaris!</h1></h2>"))
    ;; Delete outside tag:
    (sgml-with-content
     content
     (sgml-delete-tag 1)
     (should (string= "<h2>Drakaris!</h2>" (buffer-string))))
    ;; Delete inner tag:
    (sgml-with-content
     content
     (goto-char 5)                   ; position the inner tag
     (sgml-delete-tag 1)
     (should (string= "<h1>Drakaris!</h1>" (buffer-string))))))

(ert-deftest sgml-delete-tag-should-signal-error-when-deleting-too-much ()
  (let ((content "<emph>Drakaris!</emph>"))
    ;; No tags to delete:
    (sgml-with-content
     "Drakaris!"
     (should-error (sgml-delete-tag 1) :type 'error)
     (should (string= "Drakaris!" (buffer-string))))
    ;; Trying to delete 2 tags, when only 1 available:
    (sgml-with-content
     content
     (should-error (sgml-delete-tag 2) :type 'error)
     (should (string= "Drakaris!" (buffer-string))))
    ;; Trying to delete a tag, but not on/before a tag:
    (sgml-with-content
     content
     (goto-char 7)                     ; D in Drakaris
     (should-error (sgml-delete-tag 1) :type 'error)
     (should (string= content (buffer-string))))
    ;; Trying to delete a tag from position outside tag:
    (sgml-with-content
     content
     (goto-char (point-max))
     (should-error (sgml-delete-tag 1) :type 'error)
     (should (string= content (buffer-string))))))

(ert-deftest sgml-delete-tag-bug-8203-should-not-delete-apostrophe ()
  (sgml-with-content
   "<title>Winter is comin'</title>"
   (sgml-delete-tag 1)
   (should (string= "Winter is comin'" (buffer-string)))))

(ert-deftest sgml-quote-works ()
  (let ((text "Foo<Bar> \"Baz\" 'Qux'\n"))
    (with-temp-buffer
      ;; Back and forth transformation.
      (insert text)
      (sgml-quote (point-min) (point-max))
      (should (string= "Foo&lt;Bar&gt; &#34;Baz&#34; &#39;Qux&#39;\n"
                       (buffer-string)))
      (sgml-quote (point-min) (point-max) t)
      (should (string= text (buffer-string)))

      ;; The same text escaped differently.
      (erase-buffer)
      (insert "Foo&lt;Bar&gt; &#34;Baz&quot; &#x27;Qux&#X27;\n")
      (sgml-quote (point-min) (point-max) t)
      (should (string= text (buffer-string)))

      ;; Lack of semicolon.
      (erase-buffer)
      (insert "&amp&amp")
      (sgml-quote (point-min) (point-max) t)
      (should (string= "&&" (buffer-string)))

      ;; Double quoting
      (sgml-quote (point-min) (point-max))
      (sgml-quote (point-min) (point-max))
      (sgml-quote (point-min) (point-max) t)
      (sgml-quote (point-min) (point-max) t)
      (should (string= "&&" (buffer-string))))))

(ert-deftest sgml-tests--quotes-syntax ()
  (dolist (str '("a\"b <t>c'd</t>"
                 "a'b <t>c\"d</t>"
                 "<t>\"a'</t>"
                 "<t>'a\"</t>"
                 "<t>\"a'\"</t>"
                 "<t>'a\"'</t>"
                 "a\"b <tag>c'd</tag>"
                 "<tag>c>'d</tag>"
                 "<t><!-- \" --></t>"
                 "<t><!-- ' --></t>"
                 "<t>(')</t>"
                 "<t>(\")</t>"
                 ))
   (with-temp-buffer
     (sgml-mode)
     (insert str)
     (ert-info ((format "%S" str) :prefix "Test case: ")
       ;; Check that last tag is parsed as a tag.
       (should (= 1 (car (syntax-ppss (1- (point-max))))))
       (should (= 0 (car (syntax-ppss (point-max)))))))))

(ert-deftest sgml-mode-quote-in-long-text ()
  (with-temp-buffer
    (sgml-mode)
    (insert "<t>"
            ;; `syntax-propertize-wholelines' extends chunk size based
            ;; on line length, so newlines are significant!
            (make-string syntax-propertize-chunk-size ?a) "\n"
            "'"
            (make-string syntax-propertize-chunk-size ?a) "\n"
            "</t>")
    ;; If we just check (syntax-ppss (point-max)) immediately, then
    ;; we'll end up propertizing the whole buffer in one chunk (so the
    ;; test is useless).  Simulate something more like what happens
    ;; when the buffer is viewed normally.
    (cl-loop for pos from (point-min) to (point-max)
             by syntax-propertize-chunk-size
             do (syntax-ppss pos))
    (syntax-ppss (point-max))
    ;; Check that last tag is parsed as a tag.
    (should (= 1 (- (car (syntax-ppss (1- (point-max))))
                    (car (syntax-ppss (point-max))))))))

(provide 'sgml-mode-tests)
;;; sgml-mode-tests.el ends here
