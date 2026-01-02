;;; diff-mode-tests.el --- Tests for diff-mode.el  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

;; Author: Dima Kogan <dima@secretsauce.net>
;; Maintainer: emacs-devel@gnu.org

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
(require 'ert-x)
(require 'diff-mode)
(require 'diff)

(ert-deftest diff-mode-test-ignore-trailing-dashes ()
  "Check to make sure we successfully ignore trailing -- made by
'git format-patch'. This is bug #9597"

  ;; I made a test repo, put some files in it, made arbitrary changes
  ;; and invoked 'git format-patch' to get a patch out of it.  The
  ;; patch and the before and after versions of the files appear here.
  ;; The test simply tries to apply the patch.  The patch contains
  ;; trailing --, which confused diff-mode previously
  (let ((patch "From 18ed35640be496647e0a02fc155b4ee4a0490eca Mon Sep 17 00:00:00 2001
From: Dima Kogan <dima@secretsauce.net>
Date: Mon, 30 Jan 2017 22:24:13 -0800
Subject: [PATCH] test commit

---
 fil  | 3 ---
 fil2 | 4 ----
 2 files changed, 7 deletions(-)

diff --git a/fil b/fil
index 10344f1..2a56245 100644
--- a/fil
+++ b/fil
@@ -2,10 +2,8 @@ Afrocentrism
 Americanisms
 Americanization
 Americanizations
-Americanized
 Americanizes
 Americanizing
-Andrianampoinimerina
 Anglicanisms
 Antananarivo
 Apalachicola
@@ -15,6 +13,5 @@ Aristophanes
 Aristotelian
 Ashurbanipal
 Australopithecus
-Austronesian
 Bangladeshis
 Barquisimeto
diff --git a/fil2 b/fil2
index 8858f0d..86e8ea5 100644
--- a/fil2
+++ b/fil2
@@ -1,20 +1,16 @@
 whippoorwills
 whitewashing
 wholehearted
-wholeheartedly
 wholesomeness
 wildernesses
 windbreakers
 wisecracking
 withstanding
-woodcarvings
 woolgathering
 workstations
 worthlessness
 wretchedness
 wristwatches
-wrongfulness
 wrongheadedly
 wrongheadedness
-xylophonists
 youthfulness
--
2.11.0

")
        (fil_before "Afrocentrism
Americanisms
Americanization
Americanizations
Americanized
Americanizes
Americanizing
Andrianampoinimerina
Anglicanisms
Antananarivo
Apalachicola
Appalachians
Argentinians
Aristophanes
Aristotelian
Ashurbanipal
Australopithecus
Austronesian
Bangladeshis
Barquisimeto
")
        (fil_after "Afrocentrism
Americanisms
Americanization
Americanizations
Americanizes
Americanizing
Anglicanisms
Antananarivo
Apalachicola
Appalachians
Argentinians
Aristophanes
Aristotelian
Ashurbanipal
Australopithecus
Bangladeshis
Barquisimeto
")
        (fil2_before "whippoorwills
whitewashing
wholehearted
wholeheartedly
wholesomeness
wildernesses
windbreakers
wisecracking
withstanding
woodcarvings
woolgathering
workstations
worthlessness
wretchedness
wristwatches
wrongfulness
wrongheadedly
wrongheadedness
xylophonists
youthfulness
")
        (fil2_after "whippoorwills
whitewashing
wholehearted
wholesomeness
wildernesses
windbreakers
wisecracking
withstanding
woolgathering
workstations
worthlessness
wretchedness
wristwatches
wrongheadedly
wrongheadedness
youthfulness
"))
    (ert-with-temp-directory temp-dir
     (let ((buf  (find-file-noselect (format "%s/%s" temp-dir "fil" )))
           (buf2 (find-file-noselect (format "%s/%s" temp-dir "fil2"))))
       (unwind-protect
           (progn
             (with-current-buffer buf  (insert fil_before)  (save-buffer))
             (with-current-buffer buf2 (insert fil2_before) (save-buffer))

             (with-temp-buffer
               (cd temp-dir)
               (insert patch)
               (goto-char (point-min))
               (diff-apply-hunk)
               (diff-apply-hunk)
               (diff-apply-hunk))

             (should (equal (with-current-buffer buf (buffer-string))
                            fil_after))
             (should (equal (with-current-buffer buf2 (buffer-string))
                            fil2_after)))

         (ignore-errors
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)
           (with-current-buffer buf2 (set-buffer-modified-p nil))
           (kill-buffer buf2)))))))

(ert-deftest diff-mode-test-hunk-text-no-newline ()
  "Check output of `diff-hunk-text' with no newline at end of file."

  ;; First check unified change/remove/add cases with newline
  (let ((hunk "\
@@ -1 +1 @@
-foo
+bar
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo
"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar
")))

  (let ((hunk "\
@@ -1 +0,0 @@
-foo
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo
"))
    (should (equal (diff-hunk-text hunk t nil) "\
")))

  (let ((hunk "\
@@ -0,0 +1 @@
+bar
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar
")))

  ;; Check unified change/remove cases with no newline in old file
  (let ((hunk "\
@@ -1 +1 @@
-foo
\\ No newline at end of file
+bar
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar
")))

  (let ((hunk "\
@@ -1 +0,0 @@
-foo
\\ No newline at end of file
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo"))
    (should (equal (diff-hunk-text hunk t nil) "\
")))

  ;; Check unified change/add cases with no newline in new file
  (let ((hunk "\
@@ -1 +1 @@
-foo
+bar
\\ No newline at end of file
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo
"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar")))

  (let ((hunk "\
@@ -0,0 +1 @@
+bar
\\ No newline at end of file
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar")))

  ;; Check unified change case with no newline in both old/new file
  (let ((hunk "\
@@ -1 +1 @@
-foo
\\ No newline at end of file
+bar
\\ No newline at end of file
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar")))

  ;; Check context-after unified change case with no newline in both old/new file
  (let ((hunk "\
@@ -1,2 +1,2 @@
-foo
+bar
 baz
\\ No newline at end of file
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo
baz"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar
baz")))

  (let ((hunk "\
@@ -1,2 +1,2 @@
-foo
-baz
\\ No newline at end of file
+bar
+baz
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo
baz"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar
baz
")))

  (let ((hunk "\
@@ -1,2 +1,2 @@
-foo
-baz
+bar
+baz
\\ No newline at end of file
"))
    (should (equal (diff-hunk-text hunk nil nil) "\
foo
baz
"))
    (should (equal (diff-hunk-text hunk t nil) "\
bar
baz"))))

(ert-deftest diff-mode-test-font-lock ()
  "Check font-locking of diff hunks."
  ;; See comments in diff-hunk-file-names about nonascii.
  ;; In such cases, the diff-font-lock-syntax portion of this fails.
  :expected-result (if (string-match-p "[[:nonascii:]]"
                                       (ert-resource-directory))
                       :failed :passed)
  (skip-unless (executable-find shell-file-name))
  (skip-unless (executable-find diff-command))
  (let ((default-directory (ert-resource-directory))
        (old "hello_world.c")
        (new "hello_emacs.c")
        (diff-buffer (get-buffer-create "*Diff*"))
        (diff-refine 'font-lock)
        (diff-font-lock-syntax t)
        diff-beg)
    (diff-no-select old new '("-u") 'no-async diff-buffer)
    (with-current-buffer diff-buffer
      (font-lock-ensure)
      (narrow-to-region (progn (diff-hunk-next)
                               (setq diff-beg (diff-beginning-of-hunk)))
                        (diff-end-of-hunk))

      (should (equal-including-properties
               (buffer-string)
               #("@@ -1,6 +1,6 @@
 #include <stdio.h>
 int main()
 {
-  printf(\"Hello, World!\\n\");
+  printf(\"Hello, Emacs!\\n\");
   return 0;
 }
"
                 0 15 (face diff-hunk-header)
                 16 36 (face diff-context)
                 36 48 (face diff-context)
                 48 51 (face diff-context)
                 51 52 (face diff-indicator-removed)
                 52 81 (face diff-removed)
                 81 82 (face diff-indicator-added)
                 82 111 (face diff-added)
                 111 124 (face diff-context)
                 124 127 (face diff-context))))

      ;; Test diff-font-lock-syntax.
      (should (equal
               (delq nil
                     (mapcar (lambda (o)
                               (when (overlay-get o 'face)
                                 (list (- (overlay-start o) diff-beg)
                                       (- (overlay-end o) diff-beg)
                                       `( diff-mode ,(overlay-get o 'diff-mode)
                                          face ,(overlay-get o 'face)))))
                             (sort (overlays-in (point-min) (point-max))
                                   (lambda (a b)
                                     (< (overlay-start a) (overlay-start b))))))
               '((17 25 (diff-mode syntax face font-lock-preprocessor-face))
                 (26 35 (diff-mode syntax face font-lock-string-face))
                 (37 40 (diff-mode syntax face font-lock-type-face))
                 (41 45 (diff-mode syntax face font-lock-function-name-face))
                 (61 78 (diff-mode syntax face font-lock-string-face))
                 (69 74 (diff-mode fine face diff-refine-removed))
                 (91 108 (diff-mode syntax face font-lock-string-face))
                 (99 104 (diff-mode fine face diff-refine-added))
                 (114 120 (diff-mode syntax face font-lock-keyword-face))))))))

(ert-deftest diff-mode-test-font-lock-syntax-one-line ()
  "Check diff syntax highlighting for one line with no newline at end."
  :expected-result (if (string-match-p "[[:nonascii:]]"
                                       (ert-resource-directory))
                       :failed :passed)
  (skip-unless (executable-find shell-file-name))
  (skip-unless (executable-find diff-command))
  (let ((default-directory (ert-resource-directory))
        (old "hello_world_1.c")
        (new "hello_emacs_1.c")
        (diff-buffer (get-buffer-create "*Diff*"))
        (diff-refine nil)
        (diff-font-lock-syntax t)
        diff-beg)
    (diff-no-select old new '("-u") 'no-async diff-buffer)
    (with-current-buffer diff-buffer
      (font-lock-ensure)
      (narrow-to-region (progn (diff-hunk-next)
                               (setq diff-beg (diff-beginning-of-hunk)))
                        (diff-end-of-hunk))

      (should (equal-including-properties
               (buffer-string)
               #("@@ -1 +1 @@
-int main() { printf(\"Hello, World!\\n\"); return 0; }
\\ No newline at end of file
+int main() { printf(\"Hello, Emacs!\\n\"); return 0; }
\\ No newline at end of file
"
                 0 11 (face diff-hunk-header)
                 12 13 (face diff-indicator-removed)
                 13 65 (face diff-removed)
                 65 93 (face diff-context)
                 93 94 (face diff-indicator-added)
                 94 146 (face diff-added)
                 146 174 (face diff-context))))

      (should (equal (mapcar (lambda (o)
                               (list (- (overlay-start o) diff-beg)
                                     (- (overlay-end o) diff-beg)
                                     (append (and (overlay-get o 'diff-mode)
                                                  `(diff-mode ,(overlay-get o 'diff-mode)))
                                             (and (overlay-get o 'face)
                                                  `(face ,(overlay-get o 'face))))))
                             (sort (overlays-in (point-min) (point-max))
                                   (lambda (a b) (< (overlay-start a) (overlay-start b)))))
                     '((0 174 (diff-mode syntax))
                       (13 16 (diff-mode syntax face font-lock-type-face))
                       (17 21 (diff-mode syntax face font-lock-function-name-face))
                       (33 50 (diff-mode syntax face font-lock-string-face))
                       (53 59 (diff-mode syntax face font-lock-keyword-face))
                       (94 97 (diff-mode syntax face font-lock-type-face))
                       (98 102 (diff-mode syntax face font-lock-function-name-face))
                       (114 131 (diff-mode syntax face font-lock-string-face))
                       (134 140 (diff-mode syntax face font-lock-keyword-face))))))))

(ert-deftest test-hunk-file-names ()
  (with-temp-buffer
    (insert "diff -c /tmp/ange-ftp13518wvE.el /tmp/ange-ftp1351895K.el\n")
    (goto-char (point-min))
    (should (equal (diff-hunk-file-names)
                   '("/tmp/ange-ftp1351895K.el" "/tmp/ange-ftp13518wvE.el"))))
  (with-temp-buffer
    (insert "diff -c -L /ftp:slbhao:/home/albinus/src/tramp/lisp/tramp.el -L /ftp:slbhao:/home/albinus/src/emacs/lisp/net/tramp.el /tmp/ange-ftp13518wvE.el /tmp/ange-ftp1351895K.el\n")
    (goto-char (point-min))
    (should (equal (diff-hunk-file-names)
                   '("/tmp/ange-ftp1351895K.el" "/tmp/ange-ftp13518wvE.el")))))

(ert-deftest diff-mode-test-fixups-added-lines ()
  "Check that `diff-fixup-modifs' works well with hunks with added lines."
  (let ((patch "--- file
+++ file
@@ -0,0 +1,15 @@
+1
+2
+3
+4
"))
    (with-temp-buffer
      (insert patch)
      (diff-fixup-modifs (point-min) (point-max))
      (should (equal (buffer-string) "--- file
+++ file
@@ -0,0 +1,4 @@
+1
+2
+3
+4
"))))
  (let ((patch "--- file
+++ file
@@ -389,5 +398,6 @@
        while (1)
                ;
+       # not needed
        # at all
        # stop
"))
    (with-temp-buffer
      (insert patch)
      (diff-fixup-modifs (point-min) (point-max))
      (should (equal (buffer-string) "--- file
+++ file
@@ -389,4 +398,5 @@
        while (1)
                ;
+       # not needed
        # at all
        # stop
")))))

(ert-deftest diff-mode-test-fixups-empty-hunks ()
  "Check that `diff-fixup-modifs' works well with empty hunks."
  (let ((patch "--- file
+++ file
@@ -1 +1 @@
-1
@@ -10 +10 @@
-1
+1
--- otherfile
+++ otherfile
@@ -1 +1 @@
+2
@@ -10 +10 @@
-1
+1
"))
    (with-temp-buffer
      (insert patch)
      (diff-fixup-modifs (point-min) (point-max))
      (should (equal (buffer-string) "--- file
+++ file
@@ -1,1 +1,0 @@
-1
@@ -10,1 +10,1 @@
-1
+1
--- otherfile
+++ otherfile
@@ -1,0 +1,1 @@
+2
@@ -10,1 +10,1 @@
-1
+1
")))))

(defvar diff-mode-tests--git-patch
  "From 1234567890abcdef1234567890abcdef12345678 Mon Sep 17 00:00:00 2001
From: Alyssa P. Hacker <alyssa.p.hacker@example.com>
Date: Sun, 3 Mar 2025 10:30:00 -0400
Subject: [PATCH] Subtle bug fixes and slight improvements

- This is not a removed line
+ This is not an added line

---
 src/main.py | 10 +++++-----
 1 file changed, 5 insertions(+), 5 deletions(-)

diff --git a/src/main.py b/src/main.py
index 9f6c5fe43e47eab441232e54456c5c2b06297b65..7b3f91a8b4ed923c8f43183276e3ab36fe04f6c9 100644
--- a/src/main.py
+++ b/src/main.py
@@ -2,25 +2,24 @@

 def main():
     # Initialize the magic number generator
-    magic_number = 42
-    print(\"Magic number: \", magic_number)

-    # TODO: Fix the infinite loop
-    while True:
-        print(\"This loop will never end\")
+    magic_number = 73  # After reconsidering, 73 seems more appropriate
+    print(\"Updated magic number: \", magic_number)

+    # The infinite loop was probably not the best approach
+    # while True:
+    #     print(\"This loop will never end.\")

     # This part of the code handles other important tasks
     print(\"Processing other tasks...\")

     # Error handling has been updated for clarity
-    if not fixed_it_yet:
-        print(\"ERROR: Still broken!\")
+    if not fixed_it_yet:  # This should be fine now
+        print(\"ERROR: No longer an issue.\")

     # Exiting the function on a positive note
-    print(\"Goodbye, cruel world!\")
+    print(\"Goodbye, world!\")

 if __name__ == \"__main__\":
     main()

--\s
2.40.0
")

(ert-deftest diff-mode-test-git-patch ()
  (with-temp-buffer
    (insert diff-mode-tests--git-patch)
    (diff-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (re-search-forward "magic_number = 42")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'diff-removed))
    (re-search-forward "magic_number = 73")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'diff-added))))

(ert-deftest diff-mode-test-git-patch/before-first-hunk ()
  (with-temp-buffer
    (insert diff-mode-tests--git-patch)
    (diff-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (re-search-forward "This is not a removed line")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'diff-context))
    (re-search-forward "This is not an added line")
    (font-lock-ensure)
    (should (eq (get-text-property (match-beginning 0) 'face)
                'diff-context))))

(ert-deftest diff-mode-test-git-patch/signature ()
  (with-temp-buffer
    (insert diff-mode-tests--git-patch)
    (diff-mode)
    (font-lock-ensure)
    (goto-char (point-max))
    (re-search-backward "^-- $")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'diff-context))))

(ert-deftest diff-mode-test-topmost-addition-undo ()
  (let ((patch "diff --git a/fruits b/fruits
index 0dcecd3..d0eb2e7 100644
--- a/fruits
+++ b/fruits
@@ -1,2 +1,3 @@
+fruits
 apple
 orange
")
        (text-before "apple
orange
")
        (text-after "fruits
apple
orange
"))
    (ert-with-temp-directory temp-dir
      (let ((buf-after
             (find-file-noselect (format "%s/%s" temp-dir "fruits"))))
        (cd temp-dir)

        (with-current-buffer buf-after (insert text-after) (save-buffer))
        (with-temp-buffer
          (insert patch)
          (goto-char (point-min))
          (diff-hunk-next)
          ;; Undo hunk by non-nil REVERSE argument (C-u C-c C-a)
          (diff-apply-hunk t))
        (with-current-buffer buf-after
          (should (string-equal (buffer-string) text-before)))

        (with-current-buffer buf-after
          (erase-buffer) (insert text-after) (save-buffer))
        (with-temp-buffer
          (insert patch)
          (goto-char (point-min))
          (diff-hunk-next)
          ;; Undo hunk by dwim behavior
          (cl-letf (((symbol-function 'y-or-n-p) #'always))
            (diff-apply-hunk)))
        (with-current-buffer buf-after
          (should (string-equal (buffer-string) text-before)))

        (with-current-buffer buf-after
          (set-buffer-modified-p nil)
          (kill-buffer buf-after))))))

(ert-deftest diff-mode-test-bottommost-addition-undo ()
  (let ((patch "diff --git a/fruits b/fruits
index 0dcecd3..6f210ff 100644
--- a/fruits
+++ b/fruits
@@ -1,2 +1,3 @@
 apple
 orange
+plum
")
        (text-before "apple
orange
")
        (text-after "apple
orange
plum
"))
    (ert-with-temp-directory temp-dir
      (let ((buf-after
             (find-file-noselect (format "%s/%s" temp-dir "fruits"))))
        (cd temp-dir)

        (with-current-buffer buf-after (insert text-after) (save-buffer))
        (with-temp-buffer
          (insert patch)
          (goto-char (point-min))
          (diff-hunk-next)
          ;; Undo hunk by non-nil REVERSE argument (C-u C-c C-a)
          (diff-apply-hunk t))
        (with-current-buffer buf-after
          (should (string-equal (buffer-string) text-before)))

        (with-current-buffer buf-after
          (erase-buffer) (insert text-after) (save-buffer))
        (with-temp-buffer
          (insert patch)
          (goto-char (point-min))
          (diff-hunk-next)
          ;; Undo hunk by dwim behavior
          (cl-letf (((symbol-function 'y-or-n-p) #'always))
            (diff-apply-hunk)))
        (with-current-buffer buf-after
          (should (string-equal (buffer-string) text-before)))

        (with-current-buffer buf-after
          (set-buffer-modified-p nil)
          (kill-buffer buf-after))))))

(provide 'diff-mode-tests)
;;; diff-mode-tests.el ends here
