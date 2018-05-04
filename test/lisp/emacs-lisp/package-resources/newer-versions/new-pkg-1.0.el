;;; new-pkg.el --- A package only seen after "updating" archive-contents

;; Author: J. R. Hacker <jrh@example.com>
;; Version: 1.0

;;; Commentary:

;; This will only show up after updating "archive-contents".

;;; Code:

(defun new-pkg-frob ()
  "Ignore me."
  (ignore))

(provide 'new-pkg)

;;; new-pkg.el ends here
