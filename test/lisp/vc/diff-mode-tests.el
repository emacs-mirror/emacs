;; Copyright (C) 2017  Free Software Foundation, Inc

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
              (beginning-of-buffer)
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


(provide 'diff-mode-tests)
