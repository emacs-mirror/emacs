;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

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

(require 'diff-mode)
(require 'diff)

(defconst diff-mode-tests--datadir
  (expand-file-name "test/data/vc/diff-mode" source-directory))

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
")
        (temp-dir (make-temp-file "diff-mode-test" 'dir)))

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
          (kill-buffer buf2)
          (delete-directory temp-dir 'recursive))))))

(ert-deftest diff-mode-test-font-lock ()
  "Check font-locking of diff hunks."
  (skip-unless (executable-find shell-file-name))
  (skip-unless (executable-find diff-command))
  (let ((default-directory diff-mode-tests--datadir)
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

      (should (equal (mapcar (lambda (o)
                               (list (- (overlay-start o) diff-beg)
                                     (- (overlay-end o) diff-beg)
                                     (append (and (overlay-get o 'diff-mode)
                                                  `(diff-mode ,(overlay-get o 'diff-mode)))
                                             (and (overlay-get o 'face)
                                                  `(face ,(overlay-get o 'face))))))
                             (sort (overlays-in (point-min) (point-max))
                                   (lambda (a b) (< (overlay-start a) (overlay-start b)))))
                     '((0 127 (diff-mode fine))
                       (0 127 (diff-mode syntax))
                       (17 25 (diff-mode syntax face font-lock-preprocessor-face))
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
  (skip-unless (executable-find shell-file-name))
  (skip-unless (executable-find diff-command))
  (let ((default-directory diff-mode-tests--datadir)
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

(provide 'diff-mode-tests)
