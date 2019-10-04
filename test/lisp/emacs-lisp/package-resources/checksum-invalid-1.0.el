;;; checksum-invalid.el --- A package with an invalid checksum in archive-contents

;; Version: 1.0

;;; Commentary:

;; This package has an invalid checksum in archive-contents and is
;; just used to verify that package.el refuses to install.

;;; Code:

(defun p-equal-to-np-p ()
  (error "FIXME"))

(provide 'checksum-invalid)

;;; checksum-invalid.el ends here
