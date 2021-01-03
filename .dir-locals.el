;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((tab-width . 8)
         (sentence-end-double-space . t)
         (fill-column . 70)
         (bug-reference-url-format . "https://debbugs.gnu.org/%s")
         (etags-regen-program-options
          .
          ("--regex='{c}/[ 	]*DEFVAR_[A-Z_ 	(]+\"\\([^\"]+\\)\"/\\1/'"
           "--regex='{c}/[ 	]*DEFVAR_[A-Z_ 	(]+\"[^\"]+\",[ 	]\\([A-Za-z0-9_]+\\)/\\1/'"
           "--regex='{objc}/[ 	]*DEFVAR_[A-Z_ 	(]+\"\\([^\"]+\\)\"/\\1/'"
           "--regex='{objc}/[ 	]*DEFVAR_[A-Z_ 	(]+\"[^\"]+\",[ 	]\\([A-Za-z0-9_]+\\)/\\1/'")
          )))
 (c-mode . ((c-file-style . "GNU")
            (c-noise-macro-names . ("INLINE" "ATTRIBUTE_NO_SANITIZE_UNDEFINED" "UNINIT" "CALLBACK" "ALIGN_STACK"))
            (electric-quote-comment . nil)
            (electric-quote-string . nil)
            (indent-tabs-mode . t)
	    (mode . bug-reference-prog)))
 (objc-mode . ((c-file-style . "GNU")
               (electric-quote-comment . nil)
               (electric-quote-string . nil)
	       (mode . bug-reference-prog)))
 (log-edit-mode . ((log-edit-font-lock-gnu-style . t)
                   (log-edit-setup-add-author . t)))
 (change-log-mode . ((add-log-time-zone-rule . t)
		     (fill-column . 74)
		     (mode . bug-reference)))
 (diff-mode . ((mode . whitespace)))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)
                     (electric-quote-comment . nil)
                     (electric-quote-string . nil)
	             (mode . bug-reference-prog)))
 (texinfo-mode . ((electric-quote-comment . nil)
                  (electric-quote-string . nil)
	          (mode . bug-reference-prog)))
 (outline-mode . ((mode . bug-reference))))
