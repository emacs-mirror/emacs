;;; pulse-utest.el --- Tests for Pulse.
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

(require 'pulse)

;;; Code:

(defun pulse-test (&optional no-error)
  "Test the lightening function for pulsing a line.
When optional NO-ERROR Don't throw an error if we can't run tests."
  (interactive)
  (if (or (not pulse-flag) (not (pulse-available-p)))
      (if no-error
          nil
        (error (concat "Pulse test only works on versions of Emacs"
                       " that support pulsing")))
    ;; Run the tests
    (when (cedet-called-interactively-p)
      (message "<Press a key> Pulse one line.")
      (read-char))
    (pulse-momentary-highlight-one-line (point))
    (when (cedet-called-interactively-p)
      (message "<Press a key> Pulse a region.")
      (read-char))
    (pulse-momentary-highlight-region (point)
                                      (save-excursion
                                        (condition-case nil
                                            (forward-char 30)
                                          (error nil))
                                        (point)))
    (when (cedet-called-interactively-p)
      (message "<Press a key> Pulse line a specific color.")
      (read-char))
    (pulse-momentary-highlight-one-line (point) 'modeline)
    (when (cedet-called-interactively-p)
      (message "<Press a key> Pulse a pre-existing overlay.")
      (read-char))
    (let* ((start (point-at-bol))
           (end (save-excursion
                  (end-of-line)
                  (when (not (eobp))
                    (forward-char 1))
                  (point)))
           (o (make-overlay start end))
           )
      (pulse-momentary-highlight-overlay o)
      (if (overlay-buffer o)
          (delete-overlay o)
        (error "Non-temporary overlay was deleted!"))
      )
    (when (cedet-called-interactively-p)
      (message "Done!"))))


(provide 'pulse-utest)

;;; pulse.el ends here
