;;; cedet-files-utests.el ---
;;
;; Copyright (C) 2011 Eric M. Ludlam
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
;;

(require 'cedet-files)

;;; Code:

(defvar cedet-files-utest-list
  '(
    ( "/home/me/src/myproj/src/foo.c" . "!home!me!src!myproj!src!foo.c" )
    ( "c:/work/myproj/foo.el" . "!drive_c!work!myproj!foo.el" )
    ( "//windows/proj/foo.java" . "!!windows!proj!foo.java" )
    ( "/home/me/proj!bang/foo.c" . "!home!me!proj!!bang!foo.c" )
    )
  "List of different file names to test.
Each entry is a cons cell of ( FNAME . CONVERTED )
where FNAME is some file name, and CONVERTED is what it should be
converted into.")

(defun cedet-files-utest ()
  "Test out some file name conversions."
  (interactive)

  (let ((idx 0))
    (dolist (FT cedet-files-utest-list)

      (setq idx (+ idx 1))

      (let ((dir->file (cedet-directory-name-to-file-name (car FT) t))
            (file->dir (cedet-file-name-to-directory-name (cdr FT) t))
            )

        (unless (string= (cdr FT) dir->file)
          (error "Failed: %d.  Found: %S Wanted: %S"
                 idx dir->file (cdr FT))
          )

        (unless (string= file->dir (car FT))
          (error "Failed: %d.  Found: %S Wanted: %S"
                 idx file->dir (car FT))
          )

        ))))

(provide 'cedet-files-utests)

;;; cedet-files-utests.el ends here
