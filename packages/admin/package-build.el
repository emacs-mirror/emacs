(require 'package-archive)

(defun package-build-dir (pkg)
  (concat default-directory pkg))

;; So this one does the business during build
(defun package-build-prepare (dir)
  (let ((descr
         (package-desc-create :name (make-symbol dir)))
          (location (expand-file-name dir)))
      (archive-refresh-pkg-file location)
      (setq descr (package-load-descriptor location))
      (package-generate-autoloads (package-desc-name descr) location)
      (package-activate descr)
      (package--compile descr)))
