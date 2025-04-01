(defpackage :lem/link
  (:use :cl :lem :alexandria)
  (:export :link-mode
           :find-definition
           :link-open))
(in-package :lem/link)

(define-attribute link-attribute
  (t :underline t))

(defclass link ()
  ((start
    :initarg :start
    :reader link-start)
   (end
    :initarg :end
    :reader link-end)))

(defclass file-link (link)
  ((file
    :initarg :file
    :reader file-link-file)
   (line-number
    :initform nil
    :initarg :line-number
    :reader file-link-line-number)
   (charpos
    :initform nil
    :initarg :charpos
    :reader file-link-charpos)))

(defclass url-link (link)
  ((url
    :initarg :url
    :reader url-link-url)))

(define-minor-mode link-mode
    (:name "Link"
     :hide-from-modeline t
     :enable-hook 'enable
     :disable-hook 'disable))

(defun enable ()
  (add-hook (variable-value 'after-syntax-scan-hook :buffer (current-buffer))
            'scan-link))

(defun disable ()
  (remove-hook (variable-value 'after-syntax-scan-hook :buffer (current-buffer))
               'scan-link))

(defun search-file-link (point &optional limit)
  (when (search-forward-regexp point "(?:\\s|^)\\.?/\\w+" limit)
    (unless (in-string-p point)
      (assert (search-backward point "/"))
      (when (eql #\. (character-at point -1))
        (character-offset point -1))
      (with-point ((link-start point))
        (when (search-forward-regexp point "[^a-zA-Z-./_]|$" limit)
          (unless (end-line-p point)
            (character-offset point -1))
          (let ((file (points-to-string link-start point)))
            (when (probe-file file)
              (multiple-value-bind (matched-string groups)
                  (looking-at point ":(\\d+)(?::(\\d+))?")
                (cond (matched-string
                       (character-offset point (length matched-string))
                       (let ((line-number (elt groups 0))
                             (charpos (elt groups 1)))
                         (make-instance 'file-link
                                        :file file
                                        :line-number (parse-integer line-number)
                                        :charpos (when charpos (parse-integer charpos))
                                        :start link-start
                                        :end point)))
                      (t
                       (make-instance 'file-link
                                      :file file
                                      :start link-start
                                      :end point)))))))))))

(defun search-url-link (point &optional limit)
  (when (search-forward-regexp point "https?://" limit)
    (unless (in-string-p point)
      (assert (search-backward point "http"))
      (with-point ((start point))
        (skip-chars-forward point (lambda (c)
                                    (not (member c '(#\space #\tab #\newline
                                                     #\; ; for gcc's output URL: (e.g.  [8;;https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-Wbuiltin-declaration-mismatch-Wbuiltin-declaration-mismatch8;;])
                                                     )))))
        (make-instance 'url-link
                       :start start
                       :end point
                       :url (points-to-string start point))))))

(defun search-file-url-link (point &optional limit)
  (when (search-forward point "file://" limit)
    (unless (in-string-p point)
      (assert (search-backward point "file://"))
      (with-point ((start point))
        (skip-chars-forward point (lambda (c) (not (member c '(#\space #\tab #\newline)))))
        (make-instance 'file-link
                       :start start
                       :end point
                       :file (subseq (points-to-string start point) (length "file://")))))))

(defun search-link (point &optional limit)
  (with-point ((start1 point)
               (start2 point)
               (start3 point))
    (let ((link1 (search-url-link start1 limit))
          (link2 (search-file-link start2 limit))
          (link3 (search-file-url-link start3 limit)))
      (move-point point (point-max start1 start2 start3))
      (append (ensure-list link1)
              (ensure-list link2)
              (ensure-list link3)))))

(defun click-callback (window point)
  (declare (ignore window))
  (link-open point))

(defun set-link-attribute (start end)
  (put-text-property start end :sticky-attribute 'link-attribute))

(defun scan-link (start end &key (set-attribute-function #'set-link-attribute))
  (labels ((apply-link (link)
             (when set-attribute-function
               (funcall set-attribute-function (link-start link) (link-end link)))
             (put-text-property (link-start link)
                                (link-end link)
                                'link link)
             (lem-core::set-clickable (link-start link)
                                      (link-end link)
                                      'click-callback))
           (apply-all-links (searcher start end)
             (with-point ((point start))
               (loop :for link := (funcall searcher point end)
                     :while link
                     :do (apply-link link)))))
    (apply-all-links #'search-url-link start end)
    (apply-all-links #'search-file-link start end)
    (apply-all-links #'search-file-url-link start end)))

(defgeneric move-to-link (link))

(defmethod move-to-link ((link file-link))
  (let ((file (file-link-file link)))
    (when (uiop:file-exists-p file)
      (let* ((line-number (file-link-line-number link))
             (charpos (file-link-charpos link))
             (buffer (find-file-buffer file :temporary t)))
        (with-point ((point (buffer-point buffer)))
          (when line-number (move-to-line point line-number))
          (when charpos (line-offset point 0 charpos))
          point)))))

(defun github-repository-p (account-name repository)
  (not (null (ppcre:scan `(:sequence ,account-name "/" ,repository ".git" :end-anchor)
                         (str:trim
                          (uiop:run-program "git remote get-url origin"
                                            :output :string))))))

(defun number-fragment-p (uri)
  (ppcre:register-groups-bind (string)
      ("^L(\\d+)$" (quri:uri-fragment uri))
    (when string
      (parse-integer string))))


(defmethod move-to-link ((link url-link))
  (let ((uri (quri:uri (url-link-url link))))
    (when (equal "github.com" (quri:uri-host uri))
      (ppcre:register-groups-bind (account-name repository path)
          ("/([\\w-]+)/([\\w-]+)/blob/\\w+/(\\S+)" (quri:uri-path uri))
        (when (github-repository-p account-name repository)
          (when-let (line-number (number-fragment-p uri))
            (let* ((file (merge-pathnames path (lem-core/commands/project:find-root path)))
                   (buffer (find-file-buffer file :temporary t)))
              (with-point ((point (buffer-point buffer)))
                (move-to-line point line-number)
                point))))))))

(defun link-at (point)
  (text-property-at point 'link))

(defun move-to-link-at-point (point)
  (when-let ((link (link-at point)))
    (move-to-link link)))

(defun find-definition (point)
  (when-let (point (move-to-link-at-point point))
    (lem/language-mode:make-xref-location
     :filespec (buffer-filename (point-buffer point))
     :position (position-at-point point))))

(defgeneric open-link (link))

(defmethod open-link ((link file-link))
  (find-file (pathname (file-link-file link)))
  (move-to-line (current-point) (file-link-line-number link))
  (line-offset (current-point) 0 (1- (file-link-charpos link))))

(defmethod open-link ((link url-link))
  (open-external-file (url-link-url link)))

(define-command link-open (&optional (point (current-point))) ()
  (when-let (link (link-at point))
    (open-link link)))

(defun after-init-hook ()
  (link-mode t))

(add-hook *after-init-hook* 'after-init-hook)
