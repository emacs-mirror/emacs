(in-package :lem-core)

(defun maybe-load-systems (systems &key (silent t) verbose error-on-failure-p)
  "Load system SYSTEMS, potentially downloading them from an external
 repository.  SYSTEMS may be a single system or a list of
 systems. Loader behavior is modified by VERBOSE and SILENT."
  (unless (listp systems)
    (setf systems (list systems)))
  (handler-bind ((error (lambda (e)
                          (declare (ignore e))
                          (unless error-on-failure-p
                            (return-from maybe-load-systems nil)))))
      (flet ((try-load-system (system)
               (or
                (when (find-package '#:OCICL-RUNTIME)
                  (progv (list (find-symbol "*DOWNLOAD*" '#:OCICL-RUNTIME)
                               (find-symbol "*VERBOSE*" '#:OCICL-RUNTIME))
                      (list t (or verbose (not silent)))
                    (funcall (find-symbol "LOAD-SYSTEM" '#:ASDF) system)))
                (when (find-package '#:QUICKLISP)
                  (funcall (find-symbol "QUICKLOAD" '#:QUICKLISP)
                           system :verbose verbose :silent silent))
                (when (find-package '#:ASDF)
                  (funcall (find-symbol "LOAD-SYSTEM" '#:ASDF) system))
                (error "Unable to find any system-loading mechanism."))))
        (mapcar #'try-load-system systems))))
