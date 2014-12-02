
(defun yaml-expand-file (file)
  (if (not (string-match-p "/" file))
      (expand-file-name
       (concat "~/prog/c/emacs/dyn/modules/yaml/tests/" file))
    file))

(defun yaml-test-file (file)
  (require 'yaml)
  (require 'json)
  (with-current-buffer (get-buffer-create "out")
    (erase-buffer)
    (insert (json-encode (yaml-parse-file (yaml-expand-file file))))
    (json-pretty-print (point-min) (point-max))))

(defun yaml-test-buffer (file)
  (require 'yaml)
  (require 'json)
  (with-current-buffer (get-buffer-create "out")
    (erase-buffer)
    (insert (json-encode (with-temp-buffer
                           (insert-file-contents (yaml-expand-file file))
                           (yaml-parse))))
    (json-pretty-print (point-min) (point-max))))
