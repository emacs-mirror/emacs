;; meese.el --- protect the impressionable young minds of America

;; Maintainer: FSF
;; Last-Modified: 20 May 1988
;; Keywords: games

(defun protect-innocence-hook ()
  (if (and (equal (file-name-nondirectory buffer-file-name) "sex.6")
	   (not (y-or-n-p "Are you over 18? ")))
      (progn
	(clear-visited-file-modtime)
	(setq buffer-file-name (concat (file-name-directory buffer-file-name)
				       "celibacy.1"))
	(let (buffer-read-only)	; otherwise (erase-buffer) may bomb.
	  (erase-buffer)
	  (insert-file-contents buffer-file-name t))
	(rename-buffer (file-name-nondirectory buffer-file-name)))))

(or (memq 'protect-innocence-hook find-file-hooks)
    (setq find-file-hooks (cons 'protect-innocence-hook find-file-hooks)))

;;; meese.el ends here
