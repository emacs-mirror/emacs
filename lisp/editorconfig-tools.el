;;; editorconfig-tools.el --- Editorconfig tools   -*- lexical-binding: t -*-

;; Copyright (C) 2011-2026 Free Software Foundation, Inc.

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Package: editorconfig

;; See
;; https://github.com/editorconfig/editorconfig-emacs/graphs/contributors or
;; https://github.com/editorconfig/editorconfig-emacs/blob/master/CONTRIBUTORS
;; for the list of contributors.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Some utility commands for users, not used from editorconfig-mode.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'editorconfig)

;;;###autoload
(defun editorconfig-apply ()
  "Get and apply EditorConfig properties to current buffer."
  (declare (obsolete hack-local-variables "31.1"))
  (interactive)
  (when buffer-file-name
    (condition-case err
        (progn
          (let ((props (editorconfig-call-get-properties-function buffer-file-name)))
            (condition-case err
                (run-hook-with-args 'editorconfig-hack-properties-functions props)
              (error
               (display-warning '(editorconfig editorconfig-hack-properties-functions)
                                (format "Error while running editorconfig-hack-properties-functions, abort running hook: %S"
                                        err)
                                :warning)))
            (setq editorconfig-properties-hash props)
            (editorconfig-set-local-variables props)
            (editorconfig-set-coding-system-revert
             (gethash 'end_of_line props)
             (gethash 'charset props))
            (condition-case err
                (run-hook-with-args 'editorconfig-after-apply-functions props)
              (error
               (display-warning '(editorconfig editorconfig-after-apply-functions)
                                (format "Error while running editorconfig-after-apply-functions, abort running hook: %S"
                                        err)
                                :warning)))))
      (error
       (display-warning '(editorconfig editorconfig-apply)
                        (format "Error in editorconfig-apply, styles will not be applied: %S" err)
                        :error)))))

(defun editorconfig-mode-apply ()
  "Get and apply EditorConfig properties to current buffer."
  (declare (obsolete editorconfig-apply "31.1"))
  (interactive)
  (when (and major-mode buffer-file-name)
    (with-suppressed-warnings ((obsolete editorconfig-apply))
      (editorconfig-apply))))


;;;###autoload
(defun editorconfig-find-current-editorconfig ()
  "Find the closest .editorconfig file for current file."
  (interactive)
  (eval-and-compile (require 'editorconfig-core))
  (when-let* ((file (editorconfig-core-get-nearest-editorconfig
                    default-directory)))
    (find-file file)))

;;;###autoload
(defun editorconfig-display-current-properties ()
  "Display EditorConfig properties extracted for current buffer."
  (interactive)
  (if editorconfig-properties-hash
      (let ((buf (get-buffer-create "*EditorConfig Properties*"))
            (file buffer-file-name)
            (props editorconfig-properties-hash))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "# EditorConfig for %s\n" file))
          (maphash (lambda (k v)
                     (insert (format "%S = %s\n" k v)))
                   props))
        (display-buffer buf))
    (message "Properties are not applied to current buffer yet.")
    nil))
;;;###autoload
(defalias 'describe-editorconfig-properties
  #'editorconfig-display-current-properties)


(provide 'editorconfig-tools)
;;; editorconfig-tools.el ends here
