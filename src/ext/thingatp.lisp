(defpackage :lem/thingatp
  (:use :cl :lem)
  (:export
   :urlp
   :url
   :pathp
   :path))
(in-package :lem/thingatp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; URL
  (defun urlp (thing)
    (and
     (stringp thing)
     (numberp
      (ppcre:scan
       "(https://www\\.|http://www\\.|https://|http://|file://)[a-zA-Z0-9]{2,}(\\.[a-zA-Z0-9]{2,})?(\\.[a-zA-Z0-9]{2,})?" thing))))

  (deftype url ()
    `(satisfies urlp))

  ;; PATH
  (defun pathp (thing)
    (and (ppcre:scan "^(.*)\/([^\/]*)$" thing)
	 (or (uiop:directory-exists-p thing)
	     (uiop:file-exists-p thing))))

  (deftype path ()
    `(satisfies pathp)))

(define-command open-at-point () ()
  (let ((thing (symbol-string-at-point (lem:current-point))))
    (typecase thing
      (url (open-external-file thing))
      (path
       (lem:find-file (pathname thing)))
      (t
       (lem/language-mode:find-definitions)))))
