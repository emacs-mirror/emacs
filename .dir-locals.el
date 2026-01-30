;;; Directory Local Variables         -*- no-byte-compile: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((tab-width . 8)
         (sentence-end-double-space . t)
         (fill-column . 72)
	 (emacs-lisp-docstring-fill-column . 72)
         (vc-git-annotate-switches . "-w")
         (bug-reference-url-format . "https://debbugs.gnu.org/%s")
	 (diff-add-log-use-relative-names . t)
         (etags-regen-regexp-alist
          .
          ((("c" "objc") .
            ("/[ \t]*DEFVAR_[A-Z_ \t(]+\"\\([^\"]+\\)\"/\\1/"
             "/[ \t]*DEFVAR_[A-Z_ \t(]+\"[^\"]+\",[ \t]\\([A-Za-z0-9_]+\\)/\\1/"))))
         (etags-regen-ignores . ("test/manual/etags/"))
         (vc-prepare-patches-separately . nil)
         (vc-default-patch-addressee . "bug-gnu-emacs@gnu.org")
         ;; Uncomment these later once people's builds are likely to know
         ;; they're safe local variable values.
         ;; (vc-trunk-branch-regexps . ("master" "\\`emacs-[0-9]+\\'"))
         ;; (vc-topic-branch-regexps . ("\\`feature/"))
         ))
 (c-mode . ((c-file-style . "GNU")
            (c-noise-macro-names . ("INLINE" "NO_INLINE" "ATTRIBUTE_NO_SANITIZE_UNDEFINED"
                                    "ATTRIBUTE_NO_SANITIZE_ADDRESS"
                                    "UNINIT" "CALLBACK" "ALIGN_STACK" "ATTRIBUTE_MALLOC"
                                    "ATTRIBUTE_DEALLOC_FREE" "ANDROID_EXPORT" "TEST_STATIC"
                                    "INLINE_HEADER_BEGIN" "INLINE_HEADER_END"))
            (electric-quote-comment . nil)
            (electric-quote-string . nil)
            (indent-tabs-mode . t)
	    (mode . bug-reference-prog)))
 (java-mode . ((c-file-style . "GNU")
               (electric-quote-comment . nil)
               (electric-quote-string . nil)
               (indent-tabs-mode . t)
	       (mode . bug-reference-prog)))
 (objc-mode . ((c-file-style . "GNU")
               (electric-quote-comment . nil)
               (electric-quote-string . nil)
               (indent-tabs-mode . t)
	       (mode . bug-reference-prog)))
 (c-ts-mode . ((c-ts-mode-indent-style . gnu))) ;Inherits `c-mode' settings.
 (log-edit-mode . ((log-edit-font-lock-gnu-style . t)
                   (log-edit-setup-add-author . t)
		   (vc-git-log-edit-summary-target-len . 50)
                   (fill-column . 64)))
 (change-log-mode . ((add-log-time-zone-rule . t)
		     (fill-column . 74)
		     (mode . bug-reference)))
 (diff-mode . ((mode . whitespace)))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)
                     (electric-quote-comment . nil)
                     (electric-quote-string . nil)
	             (mode . bug-reference-prog)))
 (lisp-data-mode . ((indent-tabs-mode . nil)))
 (texinfo-mode . ((electric-quote-comment . nil)
                  (electric-quote-string . nil)
	          (mode . bug-reference-prog)))
 (outline-mode . ((mode . bug-reference))))
