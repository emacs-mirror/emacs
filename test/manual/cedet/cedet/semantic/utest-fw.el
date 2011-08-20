;;; utest-fw.el ---
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

;;; Code:

(defun semantic-test-data-cache ()
  "Test the data cache."
  (interactive)
  (let ((data '(a b c)))
    (save-excursion
      (set-buffer (get-buffer-create " *semantic-test-data-cache*"))
      (erase-buffer)
      (insert "The Moose is Loose")
      (goto-char (point-min))
      (semantic-cache-data-to-buffer (current-buffer) (point) (+ (point) 5)
                                     data 'moose 'exit-cache-zone)
      (if (equal (semantic-get-cache-data 'moose) data)
          (message "Successfully retrieved cached data.")
        (error "Failed to retrieve cached data"))
      )))

(defun semantic-test-throw-on-input ()
  "Test that throw on input will work."
  (interactive)
  (semantic-throw-on-input 'done-die)
  (message "Exit Code: %s"
           (semantic-exit-on-input 'testing
             (let ((inhibit-quit nil)
                   (message-log-max nil))
               (while t
                 (message "Looping ... press a key to test")
                 (semantic-throw-on-input 'test-inner-loop))
               'exit)))
  (when (input-pending-p)
    (if (fboundp 'read-event)
        (read-event)
      (read-char)))
  )


(provide 'cedet/semantic/utest-fw)

;;; utest-fw.el ends here
