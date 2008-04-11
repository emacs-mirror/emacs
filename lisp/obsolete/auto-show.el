;;; auto-show.el --- perform automatic horizontal scrolling as point moves
;;; This file is in the public domain.

;; This file is part of GNU Emacs.

;; Keywords: scroll display convenience
;; Author: Pete Ware <ware@cis.ohio-state.edu>
;; Maintainer: FSF

;;; Commentary:

;; This file has been obsolete since Emacs 21.1.

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

;; arch-tag: 49587cbf-95cc-4061-b564-274aaec37469
;;; auto-show.el ends here
