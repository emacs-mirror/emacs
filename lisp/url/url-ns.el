;;; url-ns.el --- Various netscape-ish functions for proxy definitions
;; Author: $Author: fx $
;; Created: $Date: 2000/12/20 21:08:02 $
;; Version: $Revision: 1.2 $
;; Keywords: comm, data, processes, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1997 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'url-gw)

;;;###autoload
(defun isPlainHostName (host)
  (not (string-match "\\." host)))

;;;###autoload
(defun dnsDomainIs (host dom)
  (string-match (concat (regexp-quote dom) "$") host))

;;;###autoload
(defun dnsResolve (host)
  (url-gateway-nslookup-host host))

;;;###autoload
(defun isResolvable (host)
  (if (string-match "^[0-9.]+$" host)
      t
    (not (string= host (url-gateway-nslookup-host host)))))

;;;###autoload
(defun isInNet (ip net mask)
  (let ((netc (split-string ip "\\."))
	(ipc  (split-string net "\\."))
	(maskc (split-string mask "\\.")))
    (if (or (/= (length netc) (length ipc))
	    (/= (length ipc) (length maskc)))
	nil
      (setq netc (mapcar 'string-to-int netc)
	    ipc (mapcar 'string-to-int ipc)
	    maskc (mapcar 'string-to-int maskc))
      (and
       (= (logand (nth 0 netc) (nth 0 maskc))
	  (logand (nth 0 ipc)  (nth 0 maskc)))
       (= (logand (nth 1 netc) (nth 1 maskc))
	  (logand (nth 1 ipc)  (nth 1 maskc)))
       (= (logand (nth 2 netc) (nth 2 maskc))
	  (logand (nth 2 ipc)  (nth 2 maskc)))
       (= (logand (nth 3 netc) (nth 3 maskc))
	  (logand (nth 3 ipc)  (nth 3 maskc)))))))

;; Netscape configuration file parsing
(defvar url-ns-user-prefs nil
  "Internal, do not use.")

;;;###autoload
(defun url-ns-prefs (&optional file)
  (if (not file)
      (setq file (expand-file-name "~/.netscape/preferences.js")))
  (if (not (and (file-exists-p file)
		(file-readable-p file)))
      (message "Could not open %s for reading" file)
    (save-excursion
      (let ((false nil)
	    (true t))
	(setq url-ns-user-prefs (make-hash-table :size 13 :test 'equal))
	(set-buffer (get-buffer-create " *ns-parse*"))
	(erase-buffer)
	(insert-file-contents file)
	(goto-char (point-min))
	(while (re-search-forward "^//" nil t)
	  (replace-match ";;"))
	(goto-char (point-min))
	(while (re-search-forward "^user_pref(" nil t)
	  (replace-match "(url-ns-set-user-pref "))
	(goto-char (point-min))
	(while (re-search-forward "\"," nil t)
	  (replace-match "\""))
	(goto-char (point-min))
	(eval-buffer)))))

(defun url-ns-set-user-pref (key val)
  (puthash key val url-ns-user-prefs))

;;;###autoload
(defun url-ns-user-pref (key &optional default)
  (gethash key url-ns-user-prefs default))

(provide 'url-ns)
