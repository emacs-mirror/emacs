(uiop:define-package :lem-markdown-mode/preview/html-buffer
  (:use :cl :lem)
  (:import-from :lem-markdown-mode/preview/preview
                :render))
(in-package :lem-markdown-mode/preview/html-buffer)

(defun preview-buffer-name (buffer)
  (format nil
          "*Markdown Preview ~A*"
          (buffer-name buffer)))

(defmethod lem-markdown-mode/internal:preview (buffer (view-type (eql :html-buffer)))
  (let* ((html (render (buffer-text buffer)))
         (html (format nil "
<html>
<body id=\"main\">~A</body>
</html> " html))
         (html-buffer (lem:make-html-buffer (preview-buffer-name buffer)
                                            html)))
    (pop-to-buffer html-buffer)))

(defmethod lem-markdown-mode/internal:on-save (buffer (view-type (eql :html-buffer)))
  (when (get-buffer (preview-buffer-name buffer))
    (lem-markdown-mode/internal:preview buffer :html-buffer)))

(defmethod lem-markdown-mode/internal:on-kill (buffer (view-type (eql :html-buffer)))
  )

(defmethod lem-markdown-mode/internal:on-change (buffer (view-type (eql :html-buffer)))
  (alexandria:when-let (html-buffer (get-buffer (preview-buffer-name buffer)))
    (let ((html (with-output-to-string (out)
                  (yason:encode (render (buffer-text buffer))
                                out)))
          (window (pop-to-buffer html-buffer)))
      (lem-if:js-eval (lem:implementation)
                      (window-view window)
                      (format nil "
const main = document.getElementById(\"main\");
main.innerHTML = ~A;
" html)))))
