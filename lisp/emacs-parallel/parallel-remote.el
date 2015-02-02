;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; parallel-remote.el ---

;; Copyright (C) 2013 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl)

(defvar parallel-service nil)
(defvar parallel-task-id nil)
(defvar parallel-client nil)
(defvar parallel--executed nil)
(defvar parallel-continue-when-executed nil)

(defun parallel-remote-send (data)
  (process-send-string parallel-client
                       (format "%S " (cons parallel-task-id data))))

(defun parallel-remote--init ()
  (setq parallel-client (make-network-process :name "emacs-parallel"
                                              :buffer nil
                                              :server nil
                                              :service parallel-service
                                              :host "localhost"
                                              :family 'ipv4))
  (set-process-filter parallel-client #'parallel-remote--filter)
  (parallel-remote-send 'code)
  (when noninteractive                  ; Batch Mode
    ;; The evaluation is done in the `parallel--filter' but in Batch
    ;; Mode, Emacs doesn't wait for the input, it stops as soon as
    ;; `parallel--init' has been executed.
    (while (null parallel--executed)
      (sleep-for 10))))                 ; arbitrary chosen

(defun parallel-remote--filter (_proc output)
  (dolist (code (parallel--read-output output))
    (parallel-remote-send
     (if (or noninteractive
             (not debug-on-error))
         (condition-case err
             (eval code)
           (error err))
       (eval code))))
  (unless parallel-continue-when-executed
    (setq parallel--executed t)
    (kill-emacs)))

(defun parallel--read-output (output)
  "Read lisp forms from output and return them as a list."
  (loop with output = (replace-regexp-in-string
                       "\\`[ \t\n]*" ""
                       (replace-regexp-in-string "[ \t\n]*\\'" "" output)) ; trim string
        with start = 0
        with end = (length output)
        for ret = (read-from-string output start end)
        for data = (first ret)
        do (setq start (rest ret))
        collect data
        until (= start end)))

(provide 'parallel-remote)

;;; parallel-remote.el ends here
