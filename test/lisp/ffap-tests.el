;;; ffap-tests.el --- Test suite for ffap.el -*- lexical-binding: t -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

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

(require 'cl-lib)
(require 'ert)
(require 'tramp)
(require 'ert-x)
(require 'ffap)

(ert-deftest ffap-replace-file-component ()
  (should (equal
           (ffap-replace-file-component "/ftp:who@foo.com:/whatever" "/new")
           "/ftp:who@foo.com:/new")))

(ert-deftest ffap-file-remote-p ()
  (dolist (test '(("/user@foo.bar.com:/pub" .
                   "/user@foo.bar.com:/pub")
                  ("/cssun.mathcs.emory.edu://dir" .
                   "/cssun.mathcs.emory.edu:/dir")
                  ("/ffap.el:80" .
                   "/ffap.el:80")))
    (let ((A (car test))
          (B (cdr test)))
      (should (equal (ffap-file-remote-p A) B)))))

(ert-deftest ffap-machine-p ()
  (should-not (ffap-machine-p "ftp"))
  (should-not (ffap-machine-p "nonesuch"))
  (should (eq (ffap-machine-p "ftp.mathcs.emory.edu") 'accept))
  (should-not (ffap-machine-p "mathcs" 5678))
  (should-not (ffap-machine-p "foo.bonk"))
  (should (eq (ffap-machine-p "foo.bonk.com") 'accept)))

(ert-deftest ffap-tests-25243 ()
  "Test for https://debbugs.gnu.org/25243 ."
  (ert-with-temp-file file
    :suffix "-bug25243"
    (let ((str "diff --git b/lisp/ffap.el a/lisp/ffap.el
index 3d7cebadcf..ad4b70d737 100644
--- b/lisp/ffap.el
+++ a/lisp/ffap.el
@@ -203,6 +203,9 @@ ffap-foo-at-bar-prefix
"))
      (transient-mark-mode 1)
      (when (natnump ffap-max-region-length)
        (insert
         (concat
          str
          (make-string ffap-max-region-length #xa)
          (format "%s ENDS HERE" file)))
        (call-interactively 'mark-whole-buffer)
        (should (equal "" (ffap-string-at-point)))
        (should (equal '(1 1) ffap-string-at-point-region))))))

(ert-deftest ffap-gopher-at-point ()
  (with-temp-buffer
    (insert "\
Type = 1
Name = foo
Path = /the/path
Port = 7070
Host = example.com\n")
    (should-not (ffap-gopher-at-point))
    (goto-char (point-min))
    (should (equal (ffap-gopher-at-point)
                   "gopher://example.com:7070/1/the/path"))
    (should (equal ffap-string-at-point-region
                   (list (point-min) (point-max))))
    (let ((ffap-gopher-regexp nil))
      (should-not (ffap-gopher-at-point)))))

(ert-deftest ffap-other-window--bug-25352 ()
  "Test for Bug#25352.  Checks that the window configuration is
left alone when opening a URL in an external browser."
  (cl-letf* ((old (current-window-configuration))
             (urls nil)
             (ffap-url-fetcher (lambda (url) (push url urls) nil)))
    (should-not (ffap-other-window "https://www.gnu.org"))
    (should (window-configuration-equal-p (current-window-configuration) old))
    (should (equal urls '("https://www.gnu.org")))))

(defun ffap-test-string (space string)
  (let ((ffap-file-name-with-spaces space))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (forward-char 3)
      (ffap-string-at-point))))

(ert-deftest ffap-test-with-spaces ()
  (should
   (equal
    (ffap-test-string
     t "c:/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program here.txt")
    "c:/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program here.txt"))
  (should
   (equal
    (ffap-test-string
     nil "c:/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program here.txt")
    "c:/Program"))
  (should
   (equal
    (ffap-test-string
     t "z:/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program Files/Hummingbird/")
    "z:/Program Files/Open Text Evaluation Media/Open Text Exceed 14 x86/Program Files/Hummingbird/"))
  (should
   (equal
    (ffap-test-string
     t "c:\\Program Files\\Open Text Evaluation Media\\Open Text Exceed 14 x86\\Program Files\\Hummingbird\\")
    "c:\\Program Files\\Open Text Evaluation Media\\Open Text Exceed 14 x86\\Program Files\\Hummingbird\\"))
  (should
   (equal
    (ffap-test-string
     t "d:\\Program Files\\Freescale\\CW for MPC55xx and MPC56xx 2.10\\PowerPC_EABI_Tools\\Command_Line_Tools\\CLT_Usage_Notes.txt")
    "d:\\Program Files\\Freescale\\CW for MPC55xx and MPC56xx 2.10\\PowerPC_EABI_Tools\\Command_Line_Tools\\CLT_Usage_Notes.txt"))
  (should
   (equal
    (ffap-test-string
     t "C:\\temp\\program.log on Windows or /var/log/program.log on Unix.")
    "C:\\temp\\program.log"))
  (should
   (equal
    (ffap-test-string t "~/tmp/")
    "~/tmp/"))
  (should
   (equal
    (ffap-test-string nil "~/tmp/")
    "~/tmp/"))
  (should
   (equal
    (ffap-test-string t "~abc123_áè/foo")
    "~abc123_áè/foo"))
  (should
   (equal
    (ffap-test-string t "c:/Program Files/my program.exe and here's more text")
    "c:/Program Files/my program.exe")))

(ert-deftest ffap-test-no-newlines ()
  (should-not
   (with-temp-buffer
     (save-excursion (insert "type="))
     (ffap-guess-file-name-at-point))))

(ert-deftest ffap-ido-mode ()
  (require 'ido)
  (with-temp-buffer
    (let ((ido-mode t)
          (read-file-name-function read-file-name-function)
          (read-buffer-function read-buffer-function))
      ;; Says ert-deftest:
      ;; Macros in BODY are expanded when the test is defined, not when it
      ;; is run.  If a macro (possibly with side effects) is to be tested,
      ;; it has to be wrapped in `(eval (quote ...))'.
      (eval (quote (ido-everywhere)) t)
      (let ((read-file-name-function (lambda (&rest args)
                                       (expand-file-name
                                        (nth 4 args)
                                        (nth 1 args)))))
        (save-excursion (insert "ffap-tests.el"))
        (let (kill-buffer-query-functions)
          (kill-buffer (call-interactively #'find-file-at-point)))))))

(ert-deftest ffap-test-path-unix ()
  (skip-unless (file-exists-p "/bin"))
  (skip-unless (file-exists-p "/usr/bin"))
  (with-temp-buffer
    (insert "/usr/bin:/bin")
    (goto-char (point-min))
    (should (equal (ffap-file-at-point) "/usr/bin")))
  (with-temp-buffer
    (insert "/usr/bin:/bin")
    (goto-char (point-min))
    (search-forward ":")
    (should (equal (ffap-file-at-point) "/bin")))
  (with-temp-buffer
    (insert ":/bin")
    (goto-char (point-min))
    (should (equal (ffap-file-at-point) nil))))

(ert-deftest ffap-test-path-portable ()
  ;; Why 'load-path' and not 'exec-path'?  Because there are various
  ;; complications when the test is run on Windows from MSYS Bash: the
  ;; few first directories MSYS adds to the system PATH may not exist,
  ;; and the very first one is ".", which ffap-file-at-point doesn't
  ;; recognize as a file.
  (skip-unless (> (length load-path) 2))
  (let ((dir1 (expand-file-name (car load-path)))
        (dir2 (expand-file-name (nth 1 load-path))))
    (skip-unless (and (file-exists-p dir1) (file-exists-p dir2)))
    (with-temp-buffer
      (insert (format "%s%s%s" dir1 path-separator dir2))
      (goto-char (point-min))
      ;; Use 'file-equal-p' because PATH could have backslashes, "~",
      ;; and other constructs that will make 'equal' fail.
      (should (file-equal-p (ffap-file-at-point) dir1)))
    (with-temp-buffer
      (insert (format "%s%s%s" dir1 path-separator dir2))
      (goto-char (point-min))
      (search-forward path-separator)
      (should (file-equal-p (ffap-file-at-point) dir2)))
    (with-temp-buffer
      (insert "%s%s" path-separator dir2)
      (goto-char (point-min))
      (should (equal (ffap-file-at-point) nil)))))

(ert-deftest ffap-tests--c-path ()
  (should (seq-every-p #'stringp (ffap--c-path)))
  (should (locate-file "stdio.h" (ffap--c-path)))
  (or (memq system-type '(windows-nt ms-dos))
      (should (member "/usr/include" (ffap--c-path))))
  (should (equal (ffap--c-path)
                 (delete-dups (ffap--c-path))))
  ;; Return a meaningful result even if calling some compiler fails.
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program &optional _infile _destination _display &rest _args) 1)))
    (should (seq-every-p #'stringp (ffap--c-path)))
    (should (member (expand-file-name "/usr/include")
                    (ffap--c-path)))
    (should (equal (ffap--c-path)
                   (delete-dups (ffap--c-path))))))

(ert-deftest ffap-tests--c-path/gcc-mocked ()
  ;; Handle empty values of "gcc -print-multiarch".
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program &optional _infile _destination _display &rest args)
               (when (equal (car args) "-print-multiarch")
                 (insert "\n") 0))))
    (should (member (expand-file-name "/usr/include")
                    (ffap--c-path))))
  ;; Handle single values of "gcc -print-multiarch".
  (cl-letf ((system-type 'foo)
            ((symbol-function 'call-process)
             (lambda (_program &optional _infile _destination _display &rest args)
               (when (equal (car args) "-print-multiarch")
                 (insert "x86_64-linux-gnu\n") 0))))
    (should (member (expand-file-name "/usr/include/x86_64-linux-gnu")
                    (ffap--c-path)))))

(ert-deftest ffap-tests--c-path/clang-mocked ()
  ;; Handle clang 15.0.0 output on macOS 15.2.
  (cl-letf (((symbol-function 'ffap--gcc-is-clang-p) (lambda () t))
            ((symbol-function 'call-process)
             (lambda (_program &optional _infile _destination _display &rest _args)
               (insert "\
Apple clang version 15.0.0 (clang-1500.3.9.4)
Target: arm64-apple-darwin24.2.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
 \"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang\"
[[[...Emacs test omits some verbose junk from the output here...]]]
clang -cc1 version 15.0.0 (clang-1500.3.9.4) default target arm64-apple-darwin24.2.0
ignoring nonexistent directory \"/usr/local/include\"
#include \"...\" search starts here:
#include <...> search starts here:
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/15.0.0/include
 /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include
 /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
 /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/System/Library/Frameworks (framework directory)
End of search list.
# 1 \"<stdin>\"
# 1 \"<built-in>\" 1
# 1 \"<built-in>\" 3
# 418 \"<built-in>\" 3
# 1 \"<command line>\" 1
# 1 \"<built-in>\" 2
# 1 \"<stdin>\" 2")
               0)))
    (should (member (expand-file-name "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/15.0.0/include")
                    (ffap--c-path)))))

(ert-deftest ffap-test-remote ()
  (skip-unless
   (ignore-errors
     (and
      (file-remote-p ert-remote-temporary-file-directory)
      (file-directory-p ert-remote-temporary-file-directory)
      (file-writable-p ert-remote-temporary-file-directory))))
  (let* ((ffap-prefer-remote-file t)
         (default-directory
          (expand-file-name ert-remote-temporary-file-directory))
         (temporary-file-directory default-directory)
         (test-file (make-temp-file "ffap-test")))
    (with-temp-buffer
      (insert (file-local-name test-file))
      (should (equal (ffap-file-at-point) test-file))
      (erase-buffer)
      (insert (concat "/usr/bin:" (file-local-name test-file)))
      (should (equal (ffap-file-at-point) test-file))
      (delete-file test-file)
      (should (equal (ffap-file-at-point) default-directory)))))

(provide 'ffap-tests)

;;; ffap-tests.el ends here
