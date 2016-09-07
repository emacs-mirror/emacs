(require 'package)
(require 'lisp-mnt)

;; these functions are stolen from ELPA
(defun archive--metadata (dir pkg)
  "Return a list (SIMPLE VERSION DESCRIPTION REQ EXTRAS),
where SIMPLE is non-nil if the package is simple;
VERSION is the version string of the simple package;
DESCRIPTION is the brief description of the package;
REQ is a list of requirements;
EXTRAS is an alist with additional metadata.

PKG is the name of the package and DIR is the directory where it is."
  (let* ((mainfile (expand-file-name (concat pkg ".el") dir))
         (files (directory-files dir nil "\\`dir\\'\\|\\.el\\'")))
    (setq files (delete (concat pkg "-pkg.el") files))
    (setq files (delete (concat pkg "-autoloads.el") files))
    (cond
     ((file-exists-p mainfile)
      (with-temp-buffer
	(insert-file-contents mainfile)
	(goto-char (point-min))
	(if (not (looking-at ";;;.*---[ \t]*\\(.*?\\)[ \t]*\\(-\\*-.*-\\*-[ \t]*\\)?$"))
            (error "Can't parse first line of %s" mainfile)
          ;; Grab the other fields, which are not mandatory.
          (let* ((description (match-string 1))
                 (version
                  (or (lm-header "package-version")
                      (lm-header "version")
                      (unless (equal pkg "org")
                        (error "Missing `version' header"))))
                 (_ (archive--version-to-list version)) ; Sanity check!
                 (requires-str (lm-header "package-requires"))
                 (pt (lm-header "package-type"))
                 (simple (if pt (equal pt "simple") (= (length files) 1)))
                 (keywords (lm-keywords-list))
                 (url (or (lm-header "url")
                          (format archive-default-url-format pkg)))
                 (req
                  (if requires-str
                      (mapcar 'archive--convert-require
                              (car (read-from-string requires-str))))))
            (list simple version description req
                  ;; extra parameters
                  (list (cons :url url)
                        (cons :keywords keywords)))))))
     (t
      (error "Can't find main file %s file in %s" mainfile dir)))))

;; PWL: this is changed to give a clean entry point
(defun archive--refresh-pkg-file (directory)
  (let* ((dir directory)
         (pkg (file-name-nondirectory dir)))
    (apply #'archive--write-pkg-file dir pkg
           (cdr (archive--metadata dir pkg)))))


(defun archive--write-pkg-file (pkg-dir name version desc requires extras)
  (let ((pkg-file (expand-file-name (concat name "-pkg.el") pkg-dir))
	(print-level nil)
        (print-quoted t)
	(print-length nil))
    (write-region
     (concat (format ";; Generated package description from %s.el\n"
		     name)
	     (prin1-to-string
              (nconc
               (list 'define-package
                     name
                     version
                     desc
                     (list 'quote
                           ;; Turn version lists into string form.
                           (mapcar
                            (lambda (elt)
                              (list (car elt)
                                    (package-version-join (cadr elt))))
                            requires)))
               (archive--alist-to-plist-args extras)))
	     "\n")
     nil
     pkg-file)))

(defun archive--version-to-list (vers)
  (when vers
    (let ((l (version-to-list vers)))
      ;; Signal an error for things like "1.02" which is parsed as "1.2".
      (cl-assert (equal vers (package-version-join l)) nil
                 "Unsupported version syntax %S" vers)
      l)))

(defconst archive-default-url-format "http://elpa.gnu.org/packages/%s.html")
(defun archive--alist-to-plist-args (alist)
  (mapcar (lambda (x)
            (if (and (not (consp x))
                     (or (keywordp x)
                         (not (symbolp x))
                         (memq x '(nil t))))
                x `',x))
          (apply #'nconc
                 (mapcar (lambda (pair) (list (car pair) (cdr pair))) alist))))

(defun archive--convert-require (elt)
  (list (car elt)
	(archive--version-to-list (car (cdr elt)))))


(defun package-build-dir (pkg)
  (concat default-directory pkg))

;; So this one does the business during build
(defun package-build-prepare (dir)
  (let ((descr
           (package-desc-create :name (make-symbol dir)))
          (location (package-build-dir dir)))
      (archive--refresh-pkg-file location)
      (setq descr (package-load-descriptor location))
      (package-generate-autoloads (package-desc-name descr) location)
      (package-activate descr)
      (package--compile descr)))


;; and this one does the business during the load
(defun test-load (dir)
  (setq descr
        (package-load-descriptor
         (test-dir dir)))
  (package--load-files-for-activation descr nil))

;; (test-prepare "all")
;; (test-load "all")

;; (test-prepare "metar")
;; (test-load "metar")
