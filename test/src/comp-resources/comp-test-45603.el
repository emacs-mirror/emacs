;;; -*- lexical-binding: t; -*-

;; Reduced from ivy.el.

(defvar comp-test-45603-last)
(defvar comp-test-45603-mark-prefix)
(defvar comp-test-45603-directory)
(defvar comp-test-45603-marked-candidates)

(defun comp-test-45603--call-marked (_action)
  (let* ((prefix-len (length comp-test-45603-mark-prefix))
         (marked-candidates
          (mapcar
           (lambda (s)
             (let ((cand (substring s prefix-len)))
               (if comp-test-45603-directory
                   (expand-file-name cand comp-test-45603-directory)
                 cand)))
           comp-test-45603-marked-candidates))
         (_multi-action (comp-test-45603--get-multi-action comp-test-45603-last)))
    marked-candidates))

(defalias 'comp-test-45603--file-local-name
  (if (fboundp 'file-local-name)
      #'file-local-name
    (lambda (file)
      (or (file-remote-p file 'localname) file))))

(provide 'comp-test-45603)
