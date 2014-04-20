;;; ede/detect.el --- Tests for detecting different kinds of projects.
;;
;; Copyright (C) 2014 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Code:

(require 'ede/linux)

(defvar ede-detect-utest-project-list
  '(
    ( "src/proj/Project.ede" . ede-proj-project-p )
    ( "src/automake/Makefile.am" . project-am-makefile-p )
    ( "src/android/AndroidManifest.xml" . ede-android-project-p )
    ( "src/android/src/test.java" . ede-android-project-p )
    ( "src/emacs/Makefile" . ede-emacs-project-p )
    ( "src/emacs/src/emacs.c" . ede-emacs-project-p )
    ;( "src/ant/build.xml" . ede-ant-project-p )
    ( "src/linux/Makefile" . ede-linux-project-p )
    ( "src/linux/scripts/ver_linux" . ede-linux-project-p )
     )
  "List of sources to load in ndetectable projects.
Each entry is a cons cell:
  ( SRCFILE . PROJECT-TYPE )")

;;;###autoload
(defun ede-detect-utest ()
  "Test out the detection scheme for EDE."
  (interactive)
  (save-excursion

    (let ((errlog nil)
	  (project-linux-build-directory-default 'same)
	  (project-linux-architecture-default "glnx")
	  (ede-project-directories t) ; safe to load Project.ede
	  )
      (cedet-utest-log-setup "EDE DETECT")

      (set-buffer (semantic-find-file-noselect
		   (expand-file-name "cedet/ede/detect.el"
				     cedet-utest-root)))

      (dolist (fl ede-detect-utest-project-list)

	;; Make sure we have the files we think we have.
	(when (not (file-exists-p (car fl)))
	  (error "Cannot find unit test file: %s" (car fl)))

	;; Do the detection
	(let ((fb (find-buffer-visiting (car fl)))
	      (b (semantic-find-file-noselect (car fl))))

	  (save-excursion
	    (set-buffer b)

	    ;; Run the EDE detection code.  Firing up the mode isn't really needed.
	    (ede-initialize-state-current-buffer)

	    ;; Test the result.
	    (unless (funcall (cdr fl) ede-object-root-project)

	      (message "Found %S, wanted %S"
		       ede-object-root-project
		       (cdr fl))

	      (push fl errlog))
	    )

	  ;; If it wasn't already in memory, whack it.
	  (when (and b (not fb))
	    (kill-buffer b))
	  ))

	(cedet-utest-log-shutdown
       "EDE DETECT"
       (when errlog
	 (format "%s Failures found." (length errlog))))

      (when errlog
	(error "Failures found looking for project in %s" (car (car errlog))))
      ))

  )


(provide 'cedet/ede/detect-utest)

;;; detect.el ends here
