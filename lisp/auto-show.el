;;; auto-show.el --- perform automatic horizontal scrolling as point moves
;;; This file is in the public domain.

;;; Keywords: scroll display convenience
;;; Author: Pete Ware <ware@cis.ohio-state.edu>
;;; Maintainer: FSF

;;; Commentary:

;; This file contains dummy variables and functions only because Emacs
;; does hscrolling automatically, now.

;;; Code:

(defgroup auto-show nil
  "This customization group is kept for compatibility only.
Emacs now does hscrolling automatically.  Please remove references
to auto-show from your init file and code."
  :group 'editing)

;;;###autoload
(defcustom auto-show-mode nil
  "Obsolete."
  :version "20.4"
  :type 'boolean
  :group 'auto-show)

(defcustom auto-show-shift-amount 8 
  "*Obsolete."
  :type 'integer
  :group 'auto-show)

(defcustom auto-show-show-left-margin-threshold 50
  "*Obsolete."
  :type 'integer
  :group 'auto-show)

;;;###autoload
(defun auto-show-mode (arg)
  "This command is obsolete."
  (interactive "P"))
  
(defun auto-show-make-point-visible (&optional ignore-arg)
  "This command is obsolete."
  (interactive))

(provide 'auto-show)

;; auto-show.el ends here

