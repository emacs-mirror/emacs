;;; detect-dirtest.el --- A dirtest custom EDE project
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

;;; Commentary:
;;
;; This EDE project is designed to test the dirtesting feature
;; and make sure:
;; 1) We don't load this file unless needed.
;; 2) We load the file when it is needed, and can detect the missing project.

;;; Code:

;;; NOTE: The ede-autoloader is in detect-utest.el

(defclass ede-detect-test-dirmatch-project (ede-project)
  ( )
  "A simple project for testing dirmatch.")

(defun ede-dirmatch-load (dir)
  "Return a dirmatch project object if there is one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, sinc there is only one project for a directory tree."
  (let* ((proj (ede-directory-get-open-project dir)))
    (if proj
	proj

      ;; Create a new project here.
      (let* ((name (file-name-nondirectory (directory-file-name dir)))
	     (cpp (expand-file-name (concat name ".cpp") dir)))
	(setq proj (ede-detect-test-dirmatch-project
		    name
		    :name name
		    :directory (file-name-as-directory dir)
		    :file cpp
		    :targets nil))
	;;(message "Create dirtest project type!")
	proj
	)
      )))

(defmethod ede-project-root ((this ede-detect-test-dirmatch-project))
  "Return my root."
  this)

(defmethod ede-find-target ((proj ede-detect-test-dirmatch-project) buffer)
  "Fab up a target for this test."
  (let* ((targets (oref proj targets))
	 )
    (if targets
	(car targets)
      (let* ((dir (file-name-directory (buffer-file-name buffer)))
	     (ans (ede-target
		  dir
		  :name (file-name-nondirectory
			 (directory-file-name dir))
		  :path dir
		  :source nil)))
	(object-add-to-list proj :targets ans)
	;;(message "Created target for dirtest project type!")
	ans))))

(defmethod ede-find-subproject-for-directory ((proj ede-detect-test-dirmatch-project)
					      dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

;;(message "Loaded detect-dirtest.el")

(provide 'cedet/ede/detect-dirtest)

;;; detect-dirtest.el ends here
