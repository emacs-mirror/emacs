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
