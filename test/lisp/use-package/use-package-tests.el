;;; use-package-tests.el --- Tests for use-package.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 


;;; Code:

(require 'ert)
(require 'use-package)

(ert-deftest use-package-mplist-get ()
  (let ((mplist '(:foo bar baz bal :blob plap plup :blam))
        (tests '((:foo . (bar baz bal))
                 (:blob . (plap plup))
                 (:blam . t)
                 (:blow . nil))))
    (mapc (lambda (test)
            (should
             (equal
              (use-package-mplist-get mplist
                                      (car test))
              (cdr test))))
          tests)))

(ert-deftest use-package-mplist-keys ()
  (should (equal (use-package-mplist-keys
                  '(:foo bar baz bal :blob plap plup :blam))
                 '(:foo :blob :blam))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; use-package-tests.el ends here
