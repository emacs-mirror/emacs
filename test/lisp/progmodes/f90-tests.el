;;; f90-tests.el --- tests for progmodes/f90.el

;; Copyright (C) 2011-2018 Free Software Foundation, Inc.

;; Author: Glenn Morris <rgm@gnu.org>

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

;;; Commentary:

;; This file does not have "test" in the name, because it lives under
;; a test/ directory, so that would be superfluous.

;;; Code:

(require 'ert)
(require 'f90)

(defconst f90-test-indent "\
!! Comment before code.
!!! Comments before code.
#preprocessor before code

program progname

  implicit none

  integer :: i

  !! Comment.

  do i = 1, 10

#preprocessor

     !! Comment.
     if ( i % 2 == 0 ) then
        !! Comment.
        cycle
     else
        write(*,*) i
     end if
  end do

!!! Comment.

end program progname
"
  "Test string for F90 indentation.")

(ert-deftest f90-test-indent ()
  "Test F90 indentation."
  (with-temp-buffer
    (f90-mode)
    (insert f90-test-indent)
    (indent-rigidly (point-min) (point-max) -999)
    (f90-indent-region (point-min) (point-max))
    (should (string-equal (buffer-string) f90-test-indent))))

(ert-deftest f90-test-bug3729 ()
  "Test for https://debbugs.gnu.org/3729 ."
  :expected-result :failed
  (with-temp-buffer
    (f90-mode)
    (insert "!! Comment

include \"file.f90\"

subroutine test (x)
  real x
  x = x+1.
  return
end subroutine test")
    (goto-char (point-min))
    (forward-line 2)
    (f90-indent-subprogram)
    (should (= 0 (current-indentation)))))

(ert-deftest f90-test-bug3730 ()
  "Test for https://debbugs.gnu.org/3730 ."
  (with-temp-buffer
    (f90-mode)
    (insert "a" )
    (move-to-column 68 t)
    (insert "(/ x /)")
    (f90-do-auto-fill)
    (beginning-of-line)
    (skip-chars-forward "[ \t]")
    (should (equal "&(/" (buffer-substring (point) (+ 3 (point)))))))

;; TODO bug#5593

(ert-deftest f90-test-bug8691 ()
  "Test for https://debbugs.gnu.org/8691 ."
  (with-temp-buffer
    (f90-mode)
    (insert "module modname
type, bind(c) :: type1
integer :: part1
end type type1
end module modname")
    (f90-indent-subprogram)
    (forward-line -1)
    (should (= 2 (current-indentation)))))

;; TODO bug#8812

(ert-deftest f90-test-bug8820 ()
  "Test for https://debbugs.gnu.org/8820 ."
  (with-temp-buffer
    (f90-mode)
    (should (eq (char-syntax ?%) (string-to-char ".")))))

(ert-deftest f90-test-bug9553a ()
  "Test for https://debbugs.gnu.org/9553 ."
  (with-temp-buffer
    (f90-mode)
    (insert "!!!")
    (dotimes (_i 20) (insert " aaaa"))
    (f90-do-auto-fill)
    (beginning-of-line)
    ;; This gives a more informative failure than looking-at.
    (should (equal "!!! a" (buffer-substring (point) (+ 5 (point)))))))

(ert-deftest f90-test-bug9553b ()
  "Test for https://debbugs.gnu.org/9553 ."
  (with-temp-buffer
    (f90-mode)
    (insert "!!!")
    (dotimes (_i 13) (insert " aaaa"))
    (insert "a, aaaa")
    (f90-do-auto-fill)
    (beginning-of-line)
    (should (equal "!!! a" (buffer-substring (point) (+ 5 (point)))))))

(ert-deftest f90-test-bug9690 ()
  "Test for https://debbugs.gnu.org/9690 ."
  (with-temp-buffer
    (f90-mode)
    (insert "#include \"foo.h\"")
    (f90-indent-line)
    (should (= 0 (current-indentation)))))

(ert-deftest f90-test-bug13138 ()
  "Test for https://debbugs.gnu.org/13138 ."
  (with-temp-buffer
    (f90-mode)
    (insert "program prog
  integer :: i = &
#ifdef foo
       & 1
#else
       & 2
#endif

  write(*,*) i
end program prog")
    (goto-char (point-min))
    (forward-line 2)
    (f90-indent-subprogram)
    (should (= 0 (current-indentation)))))

(ert-deftest f90-test-bug-19809 ()
  "Test for https://debbugs.gnu.org/19809 ."
  (with-temp-buffer
    (f90-mode)
    ;; The Fortran standard says that continued strings should have
    ;; '&' at the start of continuation lines, but it seems gfortran
    ;; allows them to be absent (albeit with a warning).
    (insert "program prog
  write (*,*), '&
end program prog'
end program prog")
    (goto-char (point-min))
    (f90-end-of-subprogram)
    (should (= (point) (point-max)))))

(ert-deftest f90-test-bug20680 ()
  "Test for https://debbugs.gnu.org/20680 ."
  (with-temp-buffer
    (f90-mode)
    (insert "module modname
type, extends ( sometype ) :: type1
integer :: part1
end type type1
end module modname")
    (f90-indent-subprogram)
    (forward-line -1)
    (should (= 2 (current-indentation)))))

(ert-deftest f90-test-bug20680b ()
  "Test for https://debbugs.gnu.org/20680 ."
  (with-temp-buffer
    (f90-mode)
    (insert "module modname
enum, bind(c)
enumerator :: e1 = 0
end enum
end module modname")
    (f90-indent-subprogram)
    (forward-line -1)
    (should (= 2 (current-indentation)))))

(ert-deftest f90-test-bug20969 ()
  "Test for https://debbugs.gnu.org/20969 ."
  (with-temp-buffer
    (f90-mode)
    (insert "module modname
type, extends ( sometype ), private :: type1
integer :: part1
end type type1
end module modname")
    (f90-indent-subprogram)
    (forward-line -1)
    (should (= 2 (current-indentation)))))

(ert-deftest f90-test-bug20969b ()
  "Test for https://debbugs.gnu.org/20969 ."
  (with-temp-buffer
    (f90-mode)
    (insert "module modname
type, private, extends ( sometype ) :: type1
integer :: part1
end type type1
end module modname")
    (f90-indent-subprogram)
    (forward-line -1)
    (should (= 2 (current-indentation)))))

(ert-deftest f90-test-bug21794 ()
  "Test for https://debbugs.gnu.org/21794 ."
  (with-temp-buffer
    (f90-mode)
    (insert "program prog
do i=1,10
associate (x => xa(i), y => ya(i))
a(x,y,i) = fun(x,y,i)
end associate
end do
end program prog")
    (f90-indent-subprogram)
    (forward-line -2)
    (should (= 5 (current-indentation)))))

(ert-deftest f90-test-bug25039 ()
  "Test for https://debbugs.gnu.org/25039 and 28786."
  (with-temp-buffer
    (f90-mode)
    (insert "program prog
select type (a)
type is (t1)
x = 2
class is (c1)
x = 1
class default
x=3
end select
end program prog")
    (f90-indent-subprogram)
    (forward-line -3)
    (should (= 2 (current-indentation))) ; class default
    (forward-line -2)
    (should (= 2 (current-indentation))) ; class is
    (forward-line -2)
    (should (= 2 (current-indentation))))) ; type is

;;; f90-tests.el ends here
