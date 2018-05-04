;;; messcompat.el --- making message mode compatible with mail mode

;; Copyright (C) 1996-2018 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail, news
;; Obsolete-since: 26.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file tries to provide backward compatibility with sendmail.el
;; for Message mode.  It should be used by simply adding
;;
;; (require 'messcompat)
;;
;; to the .emacs file.  Loading it after Message mode has been
;; loaded will have no effect.

;;; Code:

(require 'sendmail)

;(setq message-from-style mail-from-style)
;(setq message-interactive mail-interactive)
(setq message-setup-hook mail-setup-hook)
(setq message-mode-hook mail-mode-hook)
;(setq message-indentation-spaces mail-indentation-spaces)
;(setq message-signature mail-signature)
;(setq message-signature-file mail-signature-file)
(setq message-default-headers mail-default-headers)
(setq message-send-hook mail-send-hook)
(setq message-send-mail-function send-mail-function)

(provide 'messcompat)

;;; messcompat.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
