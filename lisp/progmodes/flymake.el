;;; flymake.el -- a universal on-the-fly syntax checker

;; Copyright (C) 2003 Free Software Foundation

;; Author:  Pavel Kobiakov <pk_at_work@yahoo.com>
;; Maintainer: Pavel Kobiakov <pk_at_work@yahoo.com>
;; Version: 0.3
;; Keywords: c languages tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Flymake is a minor Emacs mode performing on-the-fly syntax
;; checks using the external syntax check tool (for C/C++ this
;; is usually the compiler)

;;; Code:

;;;; [[ Overlay compatibility
(autoload 'make-overlay            "overlay" "Overlay compatibility kit." t)
(autoload 'overlayp                "overlay" "Overlay compatibility kit." t)
(autoload 'overlays-in             "overlay" "Overlay compatibility kit." t)
(autoload 'delete-overlay          "overlay" "Overlay compatibility kit." t)
(autoload 'overlay-put             "overlay" "Overlay compatibility kit." t)
(autoload 'overlay-get             "overlay" "Overlay compatibility kit." t)
;;;; ]]

;;;; [[ cross-emacs compatibility routines
(defvar flymake-emacs
    (cond
	((string-match "XEmacs" emacs-version)  'xemacs)
	(t                                      'emacs)
	)
    "Currently used emacs flavor"
)

(defun flymake-makehash(&optional test)
    (cond
	((equal flymake-emacs 'xemacs)  (if test (make-hash-table :test test) (make-hash-table)))
	(t                              (makehash test))	
    )
)

(defun flymake-time-to-float(&optional tm)
	"Convert `current-time` to a float number of seconds."
	(multiple-value-bind (s0 s1 s2) (or tm (current-time))
	  (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2)))
)
(defun flymake-float-time()
    (cond
	((equal flymake-emacs 'xemacs)  (flymake-time-to-float (current-time)))
	(t                              (float-time))
    )
)

(defun flymake-replace-regexp-in-string(regexp rep str)
    (cond
	((equal flymake-emacs 'xemacs)  (replace-in-string str regexp rep))
	(t                              (replace-regexp-in-string regexp rep str))
	)
)

(defun flymake-split-string-remove-empty-edges(str pattern)
    "split, then remove first and/or last in case it's empty"
	(let* ((splitted (split-string str pattern)))
	    (if (and (> (length splitted) 0) (= 0 (length (elt splitted 0))))
			(setq splitted (cdr splitted))
		)
	    (if (and (> (length splitted) 0) (= 0 (length (elt splitted (1- (length splitted))))))
			(setq splitted (reverse (cdr (reverse splitted))))
		)
		splitted
    )
)

(defun flymake-split-string(str pattern)
    (cond
	((equal flymake-emacs 'xemacs)  (flymake-split-string-remove-empty-edges str pattern))
	(t                              (flymake-split-string-remove-empty-edges str pattern))
    )
)

(defun flymake-get-temp-dir()
    (cond
	((equal flymake-emacs 'xemacs)  (temp-directory))
	(t                              temporary-file-directory)))

(defun flymake-line-beginning-position ()
    (save-excursion
	(beginning-of-line)
	(point)))

(defun flymake-line-end-position ()
    (save-excursion
	(end-of-line)
	(point)))

(defun flymake-popup-menu(pos menu-data)
    (cond
       ((equal flymake-emacs 'xemacs)
	    (let* ((x-pos       (nth 0 (nth 0 pos)))
		   (y-pos       (nth 1 (nth 0 pos)))
		   (fake-event-props  '(button 1 x 1 y 1)))
		(setq fake-event-props (plist-put fake-event-props 'x x-pos))
		(setq fake-event-props (plist-put fake-event-props 'y y-pos))
		(popup-menu (flymake-make-xemacs-menu menu-data) (make-event 'button-press fake-event-props))
	    )
       )
       (t (x-popup-menu pos (flymake-make-emacs-menu menu-data)))))

(defun flymake-make-emacs-menu(menu-data)
    (let* ((menu-title     (nth 0 menu-data))
		   (menu-items     (nth 1 menu-data))
		   (menu-commands  nil))

	(setq menu-commands (mapcar (lambda (foo)
				      (cons (nth 0 foo) (nth 1 foo)))
				    menu-items))
		(list menu-title (cons "" menu-commands))))

(defun flymake-nop ())

(defun flymake-make-xemacs-menu (menu-data)
    (let* ((menu-title     (nth 0 menu-data))
	   (menu-items     (nth 1 menu-data))
	   (menu-commands  nil))
	(setq menu-commands (mapcar (lambda (foo)
				      (vector (nth 0 foo) (or (nth 1 foo) '(flymake-nop)) t))
				    menu-items))
	(cons menu-title menu-commands)))

(defun flymake-xemacs-window-edges(&optional window)
    (let ((edges  (window-pixel-edges window))
	  tmp)
	(setq tmp edges)
	(setcar tmp (/ (car tmp) (face-width 'default)))
	(setq tmp (cdr tmp))
	(setcar tmp (/ (car tmp) (face-height 'default)))
	(setq tmp (cdr tmp))
	(setcar tmp (/ (car tmp) (face-width 'default)))
	(setq tmp (cdr tmp))
	(setcar tmp (/ (car tmp) (face-height 'default)))
	edges
    )
)

(defun flymake-current-row()
    "return current row in current frame"
    (cond
       ((equal flymake-emacs 'xemacs)  (count-lines (window-start) (point)))
       (t                              (+ (car (cdr (window-edges))) (count-lines (window-start) (point))))
    )
)

(defun flymake-selected-frame()
    (cond
       ((equal flymake-emacs 'xemacs)  (selected-window))
       (t                              (selected-frame))
    )
)

;;;; ]]

(defcustom flymake-log-level -1
    "Logging level, only messages with level > flymake-log-level will not be logged
-1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"
    :group 'flymake
    :type 'integer)

(defun flymake-log (level text &rest args)
    "Log a message with optional arguments."
    (if (<= level flymake-log-level)
	(let* ((msg (apply 'format text args)))
	    (message msg)
	    ;(with-temp-buffer
	    ;    (insert msg)
	    ;   (insert "\n")
	    ;   (flymake-save-buffer-in-file (current-buffer) "d:/flymake.log" t)  ; make log file name customizable
	    ;)
	)))

(defun flymake-ins-after (list pos val)
    "Insert VAL into LIST after position POS."
    (let ((tmp (copy-sequence list))) ; (???)
	(setcdr (nthcdr pos tmp) (cons val (nthcdr (1+ pos) tmp)))
	tmp))

(defun flymake-set-at (list pos val)
    "Set VAL at position POS in LIST"
    (let ((tmp (copy-sequence list))) ; (???)
	(setcar (nthcdr pos tmp) val)
	tmp))

(defvar flymake-pid-to-names (flymake-makehash)
  "pid -> source buffer name, output file name mapping.")

(defun flymake-reg-names (pid source-buffer-name)
    "Save into in PID map."
    (unless (stringp source-buffer-name)
	(error "Invalid buffer name"))
    (puthash pid (list source-buffer-name) flymake-pid-to-names))

(defun flymake-get-source-buffer-name (pid)
    "Return buffer name stored in PID map."
    (nth 0 (gethash pid flymake-pid-to-names)))

(defun flymake-unreg-names (pid)
    "Delete PID->buffer name mapping."
    (remhash pid flymake-pid-to-names))

(defun flymake-get-buffer-var (buffer var-name)
    "Switch to BUFFER if necessary and return local variable VAR-NAME."
    (unless (bufferp buffer)
	(error "Invalid buffer"))

    (if (eq buffer (current-buffer))
	(symbol-value var-name)
	(save-excursion
	    (set-buffer buffer)
	    (symbol-value var-name))))

(defun flymake-set-buffer-var (buffer var-name var-value)
    "Switch to BUFFER if necessary and set local variable VAR-NAME to VAR-VALUE."
    (unless (bufferp buffer)
	(error "Invalid buffer"))

    (if (eq buffer (current-buffer))
	(set var-name var-value)
	(save-excursion
	    (set-buffer buffer)
	    (set var-name var-value))))

(defvar flymake-buffer-data (flymake-makehash)
  "Data specific to syntax check tool, in name-value pairs.")

(make-variable-buffer-local 'flymake-buffer-data)

(defun flymake-get-buffer-data (buffer)
    (flymake-get-buffer-var buffer 'flymake-buffer-data))

(defun flymake-set-buffer-data (buffer data)
    (flymake-set-buffer-var buffer 'flymake-buffer-data data))

(defun flymake-get-buffer-value (buffer name)
    (gethash name (flymake-get-buffer-data buffer)))

(defun flymake-set-buffer-value (buffer name value)
    (puthash name value (flymake-get-buffer-data buffer)))

(defvar flymake-output-residual nil "")

(make-variable-buffer-local 'flymake-output-residual)

(defun flymake-get-buffer-output-residual (buffer)
    (flymake-get-buffer-var buffer 'flymake-output-residual))

(defun flymake-set-buffer-output-residual (buffer residual)
    (flymake-set-buffer-var buffer 'flymake-output-residual residual))

(defcustom flymake-allowed-file-name-masks '((".+\\.c$" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name)
					     (".+\\.cpp$" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name)
					     (".+\\.xml$" flymake-xml-init flymake-simple-cleanup flymake-get-real-file-name)
					     (".+\\.html?$" flymake-xml-init flymake-simple-cleanup flymake-get-real-file-name)
					     (".+\\.cs$" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name)
					     (".+\\.pl$" flymake-perl-init flymake-simple-cleanup flymake-get-real-file-name)
					     (".+\\.h$" flymake-master-make-header-init flymake-master-cleanup flymake-get-real-file-name)
					     (".+\\.java$" flymake-simple-make-java-init flymake-simple-java-cleanup flymake-get-real-file-name)
					     (".+[0-9]+\\.tex$" flymake-master-tex-init flymake-master-cleanup flymake-get-real-file-name)
					     (".+\\.tex$" flymake-simple-tex-init flymake-simple-cleanup flymake-get-real-file-name)
					     (".+\\.idl$" flymake-simple-make-init flymake-simple-cleanup flymake-get-real-file-name)
;                                            (".+\\.cpp$" 1)
;                                            (".+\\.java$" 3)
;                                            (".+\\.h$" 2 (".+\\.cpp$" ".+\\.c$")
;                                                ("[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\"" 1 2))
;                                            (".+\\.idl$" 1)
;                                            (".+\\.odl$" 1)
;                                            (".+[0-9]+\\.tex$" 2 (".+\\.tex$")
;                                                ("[ \t]*\\input[ \t]*{\\(.*\\)\\(%s\\)}" 1 2 ))
;                                            (".+\\.tex$" 1)
					     )
    "*Files syntax checking is allowed for."
    :group 'flymake
    :type '(repeat (string symbol symbol symbol)))

(defun flymake-get-file-name-mode-and-masks (file-name)
    "Return the corresponding entry from 'flymake-allowed-file-name-masks'."
    (unless (stringp file-name)
	(error "Invalid file-name"))
    (let ((count           (length flymake-allowed-file-name-masks))
	  (idx             0)
	  (mode-and-masks  nil))
	(while (and (not mode-and-masks) (< idx count))
	    (if (string-match (nth 0 (nth idx flymake-allowed-file-name-masks)) file-name)
		(setq mode-and-masks (cdr (nth idx flymake-allowed-file-name-masks))))
	    (setq idx (1+ idx)))
	(flymake-log 3 "file %s, init=%s" file-name (car mode-and-masks))
	mode-and-masks))

(defun flymake-can-syntax-check-file (file-name)
    "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
    (if (flymake-get-init-function file-name) t nil))

(defun flymake-get-init-function (file-name)
    "Return init function to be used for the file."
    (let* ((init-f  (nth 0 (flymake-get-file-name-mode-and-masks file-name))))
	;(flymake-log 0 "calling %s" init-f)
	;(funcall init-f (current-buffer))
	  init-f))

(defun flymake-get-cleanup-function (file-name)
    "Return cleanup function to be used for the file."
    (nth 1 (flymake-get-file-name-mode-and-masks file-name)))

(defun flymake-get-real-file-name-function (file-name)
    (or (nth 2 (flymake-get-file-name-mode-and-masks file-name)) 'flymake-get-real-file-name))

(defcustom flymake-buildfile-dirs '("." ".." "../.." "../../.." "../../../.." "../../../../.." "../../../../../.." "../../../../../../.." "../../../../../../../.." "../../../../../../../../.." "../../../../../../../../../.." "../../../../../../../../../../..")
    "Dirs to look for buildfile."
    :group 'flymake
    :type '(repeat (string)))

(defvar flymake-find-buildfile-cache (flymake-makehash 'equal))

(defun flymake-get-buildfile-from-cache (dir-name)
    (gethash dir-name flymake-find-buildfile-cache))

(defun flymake-add-buildfile-to-cache (dir-name buildfile)
    (puthash dir-name buildfile flymake-find-buildfile-cache))

(defun flymake-clear-buildfile-cache ()
    (clrhash flymake-find-buildfile-cache))

(defun flymake-find-buildfile (buildfile-name source-dir-name dirs)
    "Find buildfile starting from current directory.
Buildfile includes Makefile, build.xml etc.
Return its path if found, or nil if not found."
    (if (flymake-get-buildfile-from-cache source-dir-name)
	(progn
	    (flymake-get-buildfile-from-cache source-dir-name))
	(let* ((buildfile-dir          nil)
	       (buildfile              nil)
	       (dir-count              (length dirs))
	       (dir-idx                0)
	       (found                  nil))
	    (while (and (not found) (< dir-idx dir-count))
		(setq buildfile-dir (concat source-dir-name (nth dir-idx dirs)))
		(setq buildfile (concat buildfile-dir "/" buildfile-name))
		(when (file-exists-p buildfile)
		    (setq found t))
		(setq dir-idx (1+ dir-idx)))
	    (if found
		(progn
		    (flymake-log 3 "found buildfile at %s/%s" buildfile-dir buildfile-name)
		    (flymake-add-buildfile-to-cache source-dir-name buildfile-dir)
		    buildfile-dir)
		(progn
		    (flymake-log 3 "buildfile for %s not found" source-dir-name)
		    nil)))))

(defun flymake-fix-path-name (name)
    "Replace all occurences of '\' with '/'."
    (when name
	(let* ((new-name (flymake-replace-regexp-in-string "[\\]" "/" (expand-file-name name)))
	       (last-char (elt new-name (1- (length new-name)))))
	    (setq new-name (flymake-replace-regexp-in-string "\\./" "" new-name))
	    (if (equal "/" (char-to-string last-char))
		(setq new-name (substring new-name 0 (1- (length new-name)))))
	    new-name)))

(defun flymake-same-files (file-name-one file-name-two)
    "Check if FILE-NAME-ONE and FILE-NAME-TWO point to same file.
Return t if so, nil if not."
    (equal (flymake-fix-path-name file-name-one) (flymake-fix-path-name file-name-two)))

(defun flymake-ensure-ends-with-slash (path)
    (if (not (= (elt path (1- (length path))) (string-to-char "/")))
	(concat path "/")
	path))

(defun flymake-get-common-path-prefix (string-one string-two)
    "Return common prefix for two paths STRING-ONE and STRING-TWO."
    (when (and string-one string-two)
	(let* ((slash-pos-one  -1)
	       (slash-pos-two  -1)
	       (done           nil)
	       (prefix         nil))
	    (setq string-one (flymake-ensure-ends-with-slash string-one))
	    (setq string-two (flymake-ensure-ends-with-slash string-two))
	    (while (not done)
		(setq slash-pos-one (string-match "/" string-one (1+ slash-pos-one)))
		(setq slash-pos-two (string-match "/" string-two (1+ slash-pos-two)))
		(if (and slash-pos-one slash-pos-two
			 (= slash-pos-one slash-pos-two)
			 (string= (substring string-one 0 slash-pos-one) (substring string-two 0 slash-pos-two)))
		    (progn
			(setq prefix (substring string-one 0 (1+ slash-pos-one))))
		    (setq done t)))
	    prefix)))

(defun flymake-build-relative-path (from-dir to-dir)
    "Return rel: FROM-DIR/rel == TO-DIR."
    (if (not (equal (elt from-dir 0) (elt to-dir 0)))
	(error "First chars in paths %s, %s must be equal (same drive)" from-dir to-dir)
    ;else
	(let* ((from        (flymake-ensure-ends-with-slash (flymake-fix-path-name from-dir)))
	       (to          (flymake-ensure-ends-with-slash (flymake-fix-path-name to-dir)))
	       (prefix      (flymake-get-common-path-prefix from to))
	       (from-suffix (substring from (length prefix)))
	       (up-count    (length (flymake-split-string from-suffix "[/]")))
	       (to-suffix   (substring to   (length prefix)))
	       (idx         0)
	       (rel         nil))
	    (if (and (> (length to-suffix) 0) (equal "/" (char-to-string (elt to-suffix 0))))
		(setq to-suffix (substring to-suffix 1)))

	    (while (< idx up-count)
		(if (> (length rel) 0)
		    (setq rel (concat rel "/")))
		(setq rel (concat rel ".."))
		(setq idx (1+ idx)))
	    (if (> (length rel) 0)
		(setq rel (concat rel "/")))
	    (if (> (length to-suffix) 0)
	       (setq rel (concat rel to-suffix)))
	    (or rel "./"))))

(defcustom flymake-master-file-dirs '("." "./src" "./UnitTest")
    "Dirs where to llok for master files."
    :group 'flymake
    :type '(repeat (string)))

(defcustom flymake-master-file-count-limit 32
    "Max number of master files to check."
    :group 'flymake
    :type 'integer)

(defvar flymake-included-file-name nil " ") ; this is used to pass a parameter to a sort predicate below

(defun flymake-find-possible-master-files (file-name master-file-dirs masks)
    "Find (by name and location) all posible master files.
Mater files are .cpp and .c for and .h. Files are searched for 
starting from the .h directory and max max-level parent dirs.
File contents are not checked."
    (let* ((dir-idx    0)
	  (dir-count  (length master-file-dirs))
	  (files  nil)
	  (done   nil)
	  (masks-count (length masks)))

	(while (and (not done) (< dir-idx dir-count))
	    (let* ((dir (concat (flymake-fix-path-name (file-name-directory file-name)) "/" (nth dir-idx master-file-dirs)))
		   (masks-idx 0))
		(while (and (file-exists-p dir) (not done) (< masks-idx masks-count))
		    (let* ((mask        (nth masks-idx masks))
			   (dir-files   (directory-files dir t mask))
			   (file-count  (length dir-files))
			   (file-idx    0))

			(flymake-log 3 "dir %s, %d file(s) for mask %s" dir file-count mask)
			(while (and (not done) (< file-idx file-count))
			    (when (not (file-directory-p (nth file-idx dir-files)))
				(setq files (cons (nth file-idx dir-files) files))
				(when (>= (length files) flymake-master-file-count-limit)
				    (flymake-log 3 "master file count limit (%d) reached" flymake-master-file-count-limit)
				    (setq done t)))
			    (setq file-idx (1+ file-idx))))
		    (setq masks-idx (1+ masks-idx))))
	    (setq dir-idx (1+ dir-idx)))
	(when files
	    (setq flymake-included-file-name (file-name-nondirectory file-name))
	    (setq files (sort files 'flymake-master-file-compare))
	    (setq flymake-included-file-name nil))
	(flymake-log 3 "found %d possible master file(s)" (length files))
	files))

(defun flymake-master-file-compare (file-one file-two)
    "Compare two files speccified by FILE-ONE and FILE-TWO.
This function is used in sort to move most possible file names
to the beginning of the list (File.h -> File.cpp moved to top."
    (and (equal (file-name-sans-extension flymake-included-file-name)
		(file-name-sans-extension (file-name-nondirectory file-one)))
	 (not (equal file-one file-two))))

(defcustom flymake-check-file-limit 8192
    "Max number of chars to look at when checking possible master file."
    :group 'flymake
    :type 'integer)

(defun flymake-check-patch-master-file-buffer (master-file-temp-buffer
					    master-file-name patched-master-file-name
					    source-file-name patched-source-file-name
					    include-dirs regexp-list)
    "Check if MASTER-FILE-NAME is a master file for SOURCE-FILE-NAME.
For .cpp master file this means it includes SOURCE-FILE-NAME (.h).
If yes, patch a copy of MASTER-FILE-NAME to include PATCHED-SOURCE-FILE-NAME
instead of SOURCE-FILE-NAME.
Whether a buffer for MATER-FILE-NAME exists, use it as a source
instead of reading master file from disk."
    (let* ((found                     nil)
	   (regexp                    (format (nth 0 regexp-list) ; "[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\""
					      (file-name-nondirectory source-file-name)))
	   (path-idx                  (nth 1 regexp-list))
	   (name-idx                  (nth 2 regexp-list))
	   (inc-path                  nil)
	   (inc-name                  nil)
	   (search-limit              flymake-check-file-limit))
	(save-excursion
	    (unwind-protect
		(progn
		    (set-buffer master-file-temp-buffer)
		    (when (> search-limit (point-max))
			(setq search-limit (point-max)))
		    (flymake-log 3 "checking %s against regexp %s" master-file-name regexp)
		    (goto-char (point-min))
		    (while (and (< (point) search-limit) (re-search-forward regexp search-limit t))
						(let* ((match-beg   (match-beginning name-idx))
							   (match-end   (match-end name-idx)))

							(flymake-log 3 "found possible match for %s" (file-name-nondirectory source-file-name))
							(setq inc-path (match-string path-idx))
							(setq inc-name (match-string name-idx))
							(when (string= inc-name (file-name-nondirectory source-file-name))
								(flymake-log 3 "inc-path=%s inc-name=%s" inc-path inc-name)
								(when (flymake-check-include source-file-name inc-path inc-name include-dirs)
									(setq found t)
									; replace-match is not used here as it fails in xemacs with
									; 'last match not a buffer' error as check-includes calls replace-in-string
									(flymake-replace-region (current-buffer) match-beg match-end
															(file-name-nondirectory patched-source-file-name))))
							(forward-line 1)))
		    (when found
			(flymake-save-buffer-in-file (current-buffer) patched-master-file-name)))
		;+(flymake-log 3 "killing buffer %s" (buffer-name master-file-temp-buffer))
		(kill-buffer master-file-temp-buffer)))
	;+(flymake-log 3 "check-patch master file %s: %s" master-file-name found)
	(when found
	    (flymake-log 2 "found master file %s" master-file-name))
	found))

(defun flymake-replace-region (buffer beg end rep)
    "Replace text in BUFFER in region (BEG END) with REP."
    (save-excursion
	(delete-region beg end)
	    (goto-char beg)
	(insert rep)))

(defun flymake-read-file-to-temp-buffer (file-name)
    "Insert contents of FILE-NAME into newly created temp buffer."
    (let* ((temp-buffer (get-buffer-create (generate-new-buffer-name (concat "flymake:" (file-name-nondirectory file-name))))))
	(save-excursion
	    (set-buffer temp-buffer)
	    (insert-file-contents file-name))
	temp-buffer))

(defun flymake-copy-buffer-to-temp-buffer (buffer)
    "Copy contents of BUFFER into newly created temp buffer."
    (let ((contents     nil)
	  (temp-buffer  nil))
	(save-excursion
	    (set-buffer buffer)
	    (setq contents (buffer-string))

	    (setq temp-buffer (get-buffer-create (generate-new-buffer-name (concat "flymake:" (buffer-name buffer)))))
	    (set-buffer temp-buffer)
	    (insert contents))
	temp-buffer))

(defun flymake-check-include (source-file-name inc-path inc-name include-dirs)
    "Check if SOURCE-FILE-NAME can be found in include path.
Return t if it can be found via include path using INC-PATH and INC-NAME."
    (if (file-name-absolute-p inc-path)
	(flymake-same-files source-file-name (concat inc-path "/" inc-name))
	(let* ((count      (length include-dirs))
	       (idx        0)
	       (file-name  nil)
	       (found      nil))
	    (while (and (not found) (< idx count))
		(setq file-name (concat (file-name-directory source-file-name) "/" (nth idx include-dirs)))
		(if (> (length inc-path) 0)
		    (setq file-name (concat file-name "/" inc-path)))
		(setq file-name (concat file-name "/" inc-name))
		(when (flymake-same-files source-file-name file-name)
		    (setq found t))
		(setq idx (1+ idx)))
	    found)))

(defun flymake-find-buffer-for-file (file-name)
    "Check if there exists a buffer visiting FILE-NAME.
Return t if so, nil if not."
    (let ((buffer-name (get-file-buffer file-name)))
	(if buffer-name
	    (get-buffer buffer-name))))

(defun flymake-create-master-file (source-file-name patched-source-file-name get-incl-dirs-f create-temp-f masks include-regexp-list)
    "Save SOURCE-FILE-NAME with a different name.
Find master file, patch and save it."
    (let* ((possible-master-files     (flymake-find-possible-master-files source-file-name flymake-master-file-dirs masks))
	   (master-file-count         (length possible-master-files))
	   (idx                       0)
	   (temp-buffer               nil)
	   (master-file-name          nil)
	   (patched-master-file-name  nil)
	   (found                     nil))

	(while (and (not found) (< idx master-file-count))
	    (setq master-file-name (nth idx possible-master-files))
	    (setq patched-master-file-name (funcall create-temp-f master-file-name "flymake_master"))
	    (if (flymake-find-buffer-for-file master-file-name)
		(setq temp-buffer (flymake-copy-buffer-to-temp-buffer (flymake-find-buffer-for-file master-file-name)))
		(setq temp-buffer (flymake-read-file-to-temp-buffer master-file-name)))
	    (setq found
		  (flymake-check-patch-master-file-buffer
		       temp-buffer
		       master-file-name
		       patched-master-file-name
		       source-file-name
		       patched-source-file-name
		       (funcall get-incl-dirs-f (file-name-directory master-file-name))
		       include-regexp-list))
	    (setq idx (1+ idx)))
	(if found
	    (list master-file-name patched-master-file-name)
	    (progn
		(flymake-log 3 "none of %d master file(s) checked includes %s" master-file-count
			   (file-name-nondirectory source-file-name))
		nil))))

(defun flymake-save-buffer-in-file (buffer file-name)
    (or buffer
	(error "Invalid buffer"))
    (save-excursion
	(save-restriction
	    (set-buffer buffer)
	    (widen)
	    (make-directory (file-name-directory file-name) 1)
	    (write-region (point-min) (point-max) file-name nil 566)))
    (flymake-log 3 "saved buffer %s in file %s" (buffer-name buffer) file-name))

(defun flymake-save-string-to-file (file-name data)
    "Save string DATA to file FILE-NAME."
    (write-region data nil file-name nil 566))

(defun flymake-read-file-to-string (file-name)
    "Read contents of file FILE-NAME and return as a string."
    (with-temp-buffer
	(insert-file-contents file-name)
	(buffer-substring (point-min) (point-max))))

(defun flymake-process-filter (process output)
    "Parse OUTPUT and highlight error lines.
It's flymake process filter."
    (let* ((pid               (process-id process))
	   (source-buffer     (get-buffer (flymake-get-source-buffer-name pid))))

	(flymake-log 3 "received %d byte(s) of output from process %d" (length output) pid)
	(when source-buffer
	    (flymake-parse-output-and-residual source-buffer output))))

(defun flymake-process-sentinel (process event)
   "Sentinel for syntax check buffers."
   (if (memq (process-status process) '(signal exit))
       (let*((exit-status       (process-exit-status process))
	     (command           (process-command process))
	     (pid               (process-id process))
	     (source-buffer     (get-buffer (flymake-get-source-buffer-name pid)))
	     (cleanup-f         (flymake-get-cleanup-function (buffer-file-name source-buffer))))

	 (flymake-log 2 "process %d exited with code %d" pid exit-status)
	 (condition-case err
	     (progn
		 (flymake-log 3 "cleaning up using %s" cleanup-f)
		 (funcall cleanup-f source-buffer)

		 (flymake-unreg-names pid)
		 (delete-process process)

		 (when source-buffer
		     (save-excursion
			 (set-buffer source-buffer)

			 (flymake-parse-residual source-buffer)
			 (flymake-post-syntax-check source-buffer exit-status command)
			 (flymake-set-buffer-is-running source-buffer nil))))
	   (error
	      (let ((err-str (format "Error in process sentinel for buffer %s: %s"
				    source-buffer (error-message-string err))))
		  (flymake-log 0 err-str)
		  (flymake-set-buffer-is-running source-buffer nil)))))))

(defun flymake-post-syntax-check (source-buffer exit-status command)
   (flymake-set-buffer-err-info source-buffer (flymake-get-buffer-new-err-info source-buffer))
   (flymake-set-buffer-new-err-info source-buffer nil)

   (flymake-set-buffer-err-info source-buffer (flymake-fix-line-numbers
					     (flymake-get-buffer-err-info source-buffer)
					     1
					     (flymake-count-lines source-buffer)))
   (flymake-delete-own-overlays source-buffer)
   (flymake-highlight-err-lines source-buffer (flymake-get-buffer-err-info source-buffer))

   (let ((err-count   (flymake-get-err-count (flymake-get-buffer-err-info source-buffer) "e"))
	 (warn-count  (flymake-get-err-count (flymake-get-buffer-err-info source-buffer) "w")))

       (flymake-log 2 "%s: %d error(s), %d warning(s) in %.2f second(s)"
		  (buffer-name source-buffer) err-count warn-count
		  (- (flymake-float-time) (flymake-get-buffer-check-start-time source-buffer)))
       (flymake-set-buffer-check-start-time source-buffer nil)
       (if (and (equal 0 err-count) (equal 0 warn-count))
	   (if (equal 0 exit-status)
	       (flymake-report-status source-buffer "" "") ; PASSED
	       (if (not (flymake-get-buffer-check-was-interrupted source-buffer))
		   (flymake-report-fatal-status (current-buffer) "CFGERR"
		       (format "Configuration error has occured while running %s" command))
		   (flymake-report-status source-buffer nil ""))) ; "STOPPED"
	   (flymake-report-status source-buffer (format "%d/%d" err-count warn-count) ""))))

(defun flymake-parse-output-and-residual (source-buffer output)
    "Split OUTPUT into lines, merge in residual if necessary."
    (save-excursion
	(set-buffer source-buffer)
	(let* ((buffer-residual     (flymake-get-buffer-output-residual source-buffer))
	       (total-output        (if buffer-residual (concat buffer-residual output) output))
	       (lines-and-residual  (flymake-split-output total-output))
	       (lines               (nth 0 lines-and-residual))
	       (new-residual        (nth 1 lines-and-residual)))

	    (flymake-set-buffer-output-residual source-buffer new-residual)
	    (flymake-set-buffer-new-err-info source-buffer (flymake-parse-err-lines
							    (flymake-get-buffer-new-err-info source-buffer)
							    source-buffer lines)))))

(defun flymake-parse-residual (source-buffer)
    "Parse residual if it's non empty."
    (save-excursion
	(set-buffer source-buffer)
	(when (flymake-get-buffer-output-residual source-buffer)
	    (flymake-set-buffer-new-err-info source-buffer (flymake-parse-err-lines
							   (flymake-get-buffer-new-err-info source-buffer)
							   source-buffer
							   (list (flymake-get-buffer-output-residual source-buffer))))
	    (flymake-set-buffer-output-residual source-buffer nil))))

(defvar flymake-err-info nil
  "Sorted list of line numbers and lists of err info in the form (file, err-text).")

(make-variable-buffer-local 'flymake-err-info)

(defun flymake-get-buffer-err-info (buffer)
    (flymake-get-buffer-var buffer 'flymake-err-info))

(defun flymake-set-buffer-err-info (buffer err-info)
    (flymake-set-buffer-var buffer 'flymake-err-info err-info))

(defun flymake-er-make-er (line-no line-err-info-list)
    (list line-no line-err-info-list))

(defun flymake-er-get-line (err-info)
    (nth 0 err-info))

(defun flymake-er-get-line-err-info-list (err-info)
    (nth 1 err-info))

(defvar flymake-new-err-info nil
  "Same as 'flymake-err-info', effective when a syntax check is in progress.")

(make-variable-buffer-local 'flymake-new-err-info)

(defun flymake-get-buffer-new-err-info (buffer)
    (flymake-get-buffer-var buffer 'flymake-new-err-info))

(defun flymake-set-buffer-new-err-info (buffer new-err-info)
    (flymake-set-buffer-var buffer 'flymake-new-err-info new-err-info))

;; getters/setters for line-err-info: (file, line, type, text).
(defun flymake-ler-make-ler (file line type text &optional full-file)
    (list file line type text full-file))

(defun flymake-ler-get-file (line-err-info)
    (nth 0 line-err-info))

(defun flymake-ler-get-line (line-err-info)
    (nth 1 line-err-info))

(defun flymake-ler-get-type (line-err-info)
    (nth 2 line-err-info))

(defun flymake-ler-get-text (line-err-info)
    (nth 3 line-err-info))

(defun flymake-ler-get-full-file (line-err-info)
    (nth 4 line-err-info))

(defun flymake-ler-set-file (line-err-info file)
    (flymake-ler-make-ler file
			(flymake-ler-get-line line-err-info)
			(flymake-ler-get-type line-err-info)
			(flymake-ler-get-text line-err-info)
			(flymake-ler-get-full-file line-err-info)))

(defun flymake-ler-set-full-file (line-err-info full-file)
    (flymake-ler-make-ler (flymake-ler-get-file line-err-info)
			(flymake-ler-get-line line-err-info)
			(flymake-ler-get-type line-err-info)
			(flymake-ler-get-text line-err-info)
			full-file))

(defun flymake-ler-set-line (line-err-info line)
    (flymake-ler-make-ler (flymake-ler-get-file line-err-info)
			line
			(flymake-ler-get-type line-err-info)
			(flymake-ler-get-text line-err-info)
			(flymake-ler-get-full-file line-err-info)))

(defun flymake-get-line-err-count (line-err-info-list type)
    "Return number of errors of specified TYPE.
Value of TYPE is eigher e or w."
    (let* ((idx        0)
	   (count      (length line-err-info-list))
	   (err-count  0))

	(while (< idx count)
	    (when (equal type (flymake-ler-get-type (nth idx line-err-info-list)))
		(setq err-count (1+ err-count)))
	    (setq idx (1+ idx)))
	err-count))

(defun flymake-get-err-count (err-info-list type)
    "Return number of errors of specified TYPE for ERR-INFO-LIST."
    (let* ((idx        0)
	   (count      (length err-info-list))
	   (err-count  0))
	(while (< idx count)
	    (setq err-count (+ err-count (flymake-get-line-err-count (nth 1 (nth idx err-info-list)) type)))
	    (setq idx (1+ idx)))
	err-count))

(defun flymake-fix-line-numbers (err-info-list min-line max-line)
    "Replace line numbers with fixed value.
If line-numbers is less than MIN-LINE, set line numbers to MIN-LINE.
If line numbers is greater than MAX-LINE, set line numbers to MAX-LINE.
The reason for this fix is because some compilers might report 
line number outside the file being compiled."
    (let* ((count     (length err-info-list))
	   (err-info  nil)
	   (line      0))
	(while (> count 0)
	    (setq err-info (nth (1- count) err-info-list))
	    (setq line (flymake-er-get-line err-info))
	    (when (or (< line min-line) (> line max-line))
		(setq line (if (< line min-line) min-line max-line))
		(setq err-info-list (flymake-set-at err-info-list (1- count)
		    (flymake-er-make-er line
			(flymake-er-get-line-err-info-list err-info)))))
	    (setq count (1- count))))
    err-info-list)

(defun flymake-highlight-err-lines (buffer err-info-list)
    "Highlight error lines in BUFFER using info from ERR-INFO-LIST."
    (save-excursion
	(set-buffer buffer)
	(let* ((idx    0)
	       (count  (length err-info-list)))
	    (while (< idx count)
		(flymake-highlight-line (car (nth idx err-info-list)) (nth 1 (nth idx err-info-list)))
		(setq idx (1+ idx))))))

(defun flymake-overlay-p (ov)
     "Determine whether overlay OV was created by flymake."
     (and (overlayp ov) (overlay-get ov 'flymake-overlay)))

(defun flymake-make-overlay (beg end tooltip-text face mouse-face)
    "Allocate a flymake overlay in range BEG and END."
    (when (not (flymake-region-has-flymake-overlays beg end))
	(let ((ov (make-overlay beg end nil t t)))
	    (overlay-put ov 'face           face)
	    (overlay-put ov 'mouse-face     mouse-face)
	    (overlay-put ov 'help-echo      tooltip-text)
	    (overlay-put ov 'flymake-overlay  t)
	    (overlay-put ov 'priority 100)
	    ;+(flymake-log 3 "created overlay %s" ov)
	    ov)
	(flymake-log 3 "created an overlay at (%d-%d)" beg end)))

(defun flymake-delete-own-overlays (buffer)
    "Delete all flymake overlays in BUFFER."
    (save-excursion
	(set-buffer buffer)
	(let ((ov (overlays-in (point-min) (point-max))))
	    (while (consp ov)
		(when (flymake-overlay-p (car ov))
		    (delete-overlay (car ov))
		    ;+(flymake-log 3 "deleted overlay %s" ov)
		)
		(setq ov (cdr ov))))))

(defun flymake-region-has-flymake-overlays (beg end)
    "Check if region specified by BEG and END has overlay.
Return t if it has at least one flymake overlay, nil if no overlay."
    (let ((ov                  (overlays-in beg end))
	  (has-flymake-overlays  nil))
	(while (consp ov)
	    (when (flymake-overlay-p (car ov))
		(setq has-flymake-overlays t))
	    (setq ov (cdr ov)))))

(defface flymake-errline-face
;+   '((((class color)) (:foreground "OrangeRed" :bold t :underline t))
;+   '((((class color)) (:underline "OrangeRed"))
   '((((class color)) (:background "LightPink"))
     (t (:bold t)))
   "Face used for marking error lines."
    :group 'flymake)

(defface flymake-warnline-face
   '((((class color)) (:background "LightBlue2"))
     (t (:bold t)))
   "Face used for marking warning lines."
    :group 'flymake)

(defun flymake-highlight-line (line-no line-err-info-list)
    "Highlight line LINE-NO in current buffer.
Perhaps use text from LINE-ERR-INFO-ILST to enhance highlighting."
    (goto-line line-no)
    (let* ((line-beg (flymake-line-beginning-position))
	   (line-end (flymake-line-end-position))
	   (beg      line-beg)
	   (end      line-end)
	   (tooltip-text (flymake-ler-get-text (nth 0 line-err-info-list)))
	   (face     nil))

	(goto-char line-beg)
	(while (looking-at "[ \t]")
	    (forward-char))

	(setq beg (point))

	(goto-char line-end)
	(while (and (looking-at "[ \t\r\n]") (> (point) 1))
	    (backward-char))

	(setq end (1+ (point)))

	(when (<= end beg)
	    (setq beg line-beg)
	    (setq end line-end))

	(when (= end beg)
	    (goto-char end)
	    (forward-line)
	    (setq end (point)))

	(if (> (flymake-get-line-err-count line-err-info-list "e") 0)
	    (setq face 'flymake-errline-face)
	    (setq face 'flymake-warnline-face))

	(flymake-make-overlay beg end tooltip-text face nil)))

(defun flymake-parse-err-lines (err-info-list source-buffer lines)
    "Parse err LINES, store info in ERR-INFO-LIST."
    (let* ((count              (length lines))
	   (idx                0)
	   (line-err-info      nil)
	   (real-file-name     nil)
	   (source-file-name   (buffer-file-name source-buffer))
	   (get-real-file-name-f (flymake-get-real-file-name-function source-file-name)))

	(while (< idx count)
	    (setq line-err-info (flymake-parse-line (nth idx lines)))
	    (when line-err-info
		(setq real-file-name (funcall get-real-file-name-f source-buffer (flymake-ler-get-file line-err-info)))
		(setq line-err-info (flymake-ler-set-full-file line-err-info real-file-name))

		(if (flymake-same-files real-file-name source-file-name)
		    (setq line-err-info (flymake-ler-set-file line-err-info nil))
		    (setq line-err-info (flymake-ler-set-file line-err-info (file-name-nondirectory real-file-name))))

		(setq err-info-list (flymake-add-err-info err-info-list line-err-info)))
	    (flymake-log 3 "parsed '%s', %s line-err-info" (nth idx lines) (if line-err-info "got" "no"))
	    (setq idx (1+ idx)))
	err-info-list))

(defun flymake-split-output (output)
    "Split OUTPUT into lines.
Return last one as residual if it does not end with newline char. Returns ((lines) residual)."
    (when (and output (> (length output) 0))
	(let* ((lines (flymake-split-string output "[\n\r]+"))
	       (complete (equal "\n" (char-to-string (aref output (1- (length output))))))
	       (residual nil))
	    (when (not complete)
		(setq residual (car (last lines)))
		(setq lines (butlast lines)))
	    (list lines residual))))

(defun flymake-reformat-err-line-patterns-from-compile-el (original-list)
    "Grab error line patterns from ORIGINAL-LIST in compile.el format.
Convert it to flymake internal format."
	(let* ((converted-list '()))
	(mapcar
	    (lambda (item)
			(setq item (cdr item))
			(let ((regexp (nth 0 item))
				  (file (nth 1 item))
				  (line (nth 2 item))
				  (col (nth 3 item))
				  end-line)
			  (if (consp file)	(setq file (car file)))
			  (if (consp line)	(setq end-line (cdr line) line (car line)))
			  (if (consp col)	(setq col (car col)))

			  (when (not (functionp line))
				  (setq converted-list (cons (list regexp file line col) converted-list)))))
		original-list)
	converted-list))

(eval-when-compile
    (require 'compile))

(defvar flymake-err-line-patterns  ; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
    (append
     '(
    ; MS Visual C++ 6.0
       ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
	1 3 nil 4)
    ; jikes
       ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\|Semantic Error\\):[ \t\n]*\\(.+\\)\\)"
	1 3 nil 4)
    ; MS midl
       ("midl[ ]*:[ ]*\\(command line error .*\\)"
	nil nil nil 1)
    ; MS C#
       ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\),[0-9]+)\: \\(\\(error\\|warning\\|fatal error\\) \\(CS[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
	1 3 nil 4)
    ; perl
       ("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)
    ; LaTeX warnings (fileless) ("\\(LaTeX \\(Warning\\|Error\\): .*\\) on input line \\([0-9]+\\)" 20 3 nil 1)
    ; ant/javac
	   (" *\\(\\[javac\\]\\)? *\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[ \t\n]*\\(.+\\)"
	2 4 nil 5))
	 ;; compilation-error-regexp-alist)
     (flymake-reformat-err-line-patterns-from-compile-el compilation-error-regexp-alist-alist)) 
    "patterns for matching error/warning lines, (regexp file-idx line-idx err-text-idx). Use flymake-reformat-err-line-patterns-from-compile-el to add patterns from compile.el")

;(defcustom flymake-err-line-patterns
;  '(
;    ; MS Visual C++ 6.0
;    ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
;       1 3 4)
;   ; jikes
;   ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\):[ \t\n]*\\(.+\\)\\)"
;       1 3 4))
;    "patterns for matching error/warning lines, (regexp file-idx line-idx err-text-idx)"
;   :group 'flymake
;   :type '(repeat (string number number number))
;)

(defun flymake-parse-line (line)
    "Parse LINE to see if it is an error of warning.
Return its components if so, nil if no."
    (let ((raw-file-name nil)
	  (line-no 0)
	  (err-type "e")
	  (err-text nil)
	  (count (length flymake-err-line-patterns))
	  (idx   0)
	  (matched nil))
	(while (and (< idx count) (not matched))
	    (when (string-match (car (nth idx flymake-err-line-patterns)) line)
		(let* ((file-idx (nth 1 (nth idx flymake-err-line-patterns)))
		       (line-idx (nth 2 (nth idx flymake-err-line-patterns))))

		    (setq raw-file-name (if file-idx (match-string file-idx line) nil))
		    (setq line-no       (if line-idx (string-to-int (match-string line-idx line)) 0))
		    (setq err-text      (if (> (length (nth idx flymake-err-line-patterns)) 4)
					    (match-string (nth 4 (nth idx flymake-err-line-patterns)) line)
					  (flymake-patch-err-text (substring line (match-end 0)))))
		    (or err-text (setq err-text "<no error text>"))
		    (if (and err-text (string-match "^[wW]arning" err-text))
			(setq err-type "w")
		    )
		    (flymake-log 3 "parse line: file-idx=%s line-idx=%s file=%s line=%s text=%s" file-idx line-idx
				 raw-file-name line-no err-text)
		    (setq matched t)))
	    (setq idx (1+ idx)))
	(if matched
	   (flymake-ler-make-ler raw-file-name line-no err-type err-text)
	   ())))

(defun flymake-find-err-info (err-info-list line-no)
    "Find (line-err-info-list pos) for specified LINE-NO."
    (if err-info-list
	(let* ((line-err-info-list  nil)
	       (pos       0)
	       (count     (length err-info-list)))

	    (while (and (< pos count) (< (car (nth pos err-info-list)) line-no))
		(setq pos (1+ pos)))
	    (when (and (< pos count) (equal (car (nth pos err-info-list)) line-no))
		(setq line-err-info-list (flymake-er-get-line-err-info-list (nth pos err-info-list))))
	    (list line-err-info-list pos))
	'(nil 0)))

(defun flymake-line-err-info-is-less-or-equal (line-one line-two)
    (or (string< (flymake-ler-get-type line-one) (flymake-ler-get-type line-two))
	(and (string= (flymake-ler-get-type line-one) (flymake-ler-get-type line-two))
	     (not (flymake-ler-get-file line-one)) (flymake-ler-get-file line-two))
	(and (string= (flymake-ler-get-type line-one) (flymake-ler-get-type line-two))
	     (or (and      (flymake-ler-get-file line-one)       (flymake-ler-get-file line-two))
		 (and (not (flymake-ler-get-file line-one)) (not (flymake-ler-get-file line-two)))))))

(defun flymake-add-line-err-info (line-err-info-list line-err-info)
    "Insert new err info favoring sorting: err-type e/w, filename nil/non-nil."
    (if (not line-err-info-list)
	(list line-err-info)
	(let* ((count  (length line-err-info-list))
	       (idx    0))
	    (while (and (< idx count) (flymake-line-err-info-is-less-or-equal (nth idx line-err-info-list) line-err-info))
		(setq idx (1+ idx)))
	    (cond ((equal 0     idx)    (setq line-err-info-list (cons line-err-info line-err-info-list)))
		  (t                    (setq line-err-info-list (flymake-ins-after line-err-info-list (1- idx) line-err-info))))
	    line-err-info-list)))

(defun flymake-add-err-info (err-info-list line-err-info)
    "Add error info (file line type text) to err info list preserving sort order."
    (let* ((count               (length err-info-list))
	   (line-no             (if (flymake-ler-get-file line-err-info) 1 (flymake-ler-get-line line-err-info)))
	   (info-and-pos        (flymake-find-err-info err-info-list line-no))
	   (exists              (car info-and-pos))
	   (pos                 (nth 1 info-and-pos))
	   (line-err-info-list  nil)
	   (err-info            nil))

	(if exists
	    (setq line-err-info-list (flymake-er-get-line-err-info-list (car (nthcdr pos err-info-list)))))
	(setq line-err-info-list (flymake-add-line-err-info line-err-info-list line-err-info))

	(setq err-info (flymake-er-make-er line-no line-err-info-list))
	(cond (exists             (setq err-info-list (flymake-set-at err-info-list pos err-info)))
	      ((equal 0 pos)      (setq err-info-list (cons err-info err-info-list)))
	      (t                  (setq err-info-list (flymake-ins-after err-info-list (1- pos) err-info))))
	err-info-list))

(defun flymake-get-project-include-dirs-imp (basedir)
    "Include dirs for the project current file belongs to."
    (if (flymake-get-project-include-dirs-from-cache basedir)
	(progn
	    (flymake-get-project-include-dirs-from-cache basedir))
    ;else
	(let* ((command-line  (concat "make -C\"" basedir "\" DUMPVARS=INCLUDE_DIRS dumpvars"))
	       (output        (shell-command-to-string command-line))
	       (lines         (flymake-split-string output "\n"))
	       (count         (length lines))
	       (idx           0)
	       (inc-dirs      nil))
	    (while (and (< idx count) (not (string-match "^INCLUDE_DIRS=.*" (nth idx lines))))
	       (setq idx (1+ idx)))
	    (when (< idx count)
		(let* ((inc-lines  (flymake-split-string (nth idx lines) " *-I"))
		       (inc-count  (length inc-lines)))
		    (while (> inc-count 0)
			(when (not (string-match "^INCLUDE_DIRS=.*" (nth (1- inc-count) inc-lines)))
			    (setq inc-dirs (cons (flymake-replace-regexp-in-string "\"" "" (nth (1- inc-count) inc-lines)) inc-dirs)))
			(setq inc-count (1- inc-count)))))
	    (flymake-add-project-include-dirs-to-cache basedir inc-dirs)
	    inc-dirs)))

(defcustom flymake-get-project-include-dirs-function 'flymake-get-project-include-dirs-imp
    "Function used to get project inc dirs, one paramater: basedir name."
    :group 'flymake
    :type 'function)

(defun flymake-get-project-include-dirs (basedir)
    (funcall flymake-get-project-include-dirs-function basedir))

(defun flymake-get-system-include-dirs ()
    "System include dirs - from the 'INCLUDE' env setting."
    (let* ((includes (getenv "INCLUDE")))
	(if includes (flymake-split-string includes path-separator) nil)))

(defvar flymake-project-include-dirs-cache (flymake-makehash 'equal))

(defun flymake-get-project-include-dirs-from-cache (base-dir)
    (gethash base-dir flymake-project-include-dirs-cache))

(defun flymake-add-project-include-dirs-to-cache (base-dir include-dirs)
    (puthash base-dir include-dirs flymake-project-include-dirs-cache))

(defun flymake-clear-project-include-dirs-cache ()
    (clrhash flymake-project-include-dirs-cache))

(defun flymake-get-include-dirs (base-dir)
    "Get dirs to use when resolving local file names."
    (let* ((include-dirs (append '(".") (flymake-get-project-include-dirs base-dir) (flymake-get-system-include-dirs))))
	include-dirs))

(defun flymake-find-file (rel-file-name include-dirs)
    "Iterate through include-dirs to find file REL-FILE-NAME.
Return first 'INCLUDE-DIRS/REL-FILE-NAME' that exists,  or just REL-FILE-NAME if not."
    (let* ((count          (length include-dirs))
	   (idx            0)
	   (found          nil)
	   (full-file-name rel-file-name))

	(while (and (not found) (< idx count))
	    (let* ((dir (nth idx include-dirs)))
		(setq full-file-name  (concat dir "/" rel-file-name))
		(when (file-exists-p full-file-name)
		    (setq found t)))
	    (setq idx (1+ idx)))
	(if found
	    full-file-name
	    rel-file-name)))

(defun flymake-restore-formatting (source-buffer)
    "Remove any formatting made by flymake."
)

(defun flymake-get-program-dir (buffer)
    "Get dir to start program in."
    (unless (bufferp buffer)
	(error "Invlid buffer"))
    (save-excursion
	(set-buffer buffer)
	default-directory))

(defun flymake-safe-delete-file (file-name)
    (when (and file-name (file-exists-p file-name))
	(delete-file file-name)
	(flymake-log 1 "deleted file %s" file-name)))

(defun flymake-safe-delete-directory (dir-name)
    (condition-case err
	(progn
	    (delete-directory dir-name)
	    (flymake-log 1 "deleted dir %s" dir-name))
	(error
	    (flymake-log 1 "Failed to delete dir %s, error ignored" dir-name))))

(defcustom flymake-compilation-prevents-syntax-check t 
     "If non-nil, syntax check won't be started in case compilation is running."
    :group 'flymake
    :type 'boolean)

(defun flymake-start-syntax-check (buffer)
    "Start syntax checking for buffer BUFFER."
    (unless (bufferp buffer)
	(error "Expected a buffer"))
    (save-excursion
	(set-buffer buffer)
	(flymake-log 3 "flymake is running: %s" (flymake-get-buffer-is-running buffer))
	(when (and (not (flymake-get-buffer-is-running buffer))
		   (flymake-can-syntax-check-file (buffer-file-name buffer)))
	    (when (or (not flymake-compilation-prevents-syntax-check)
		    (not (flymake-compilation-is-running))) ;+ (flymake-rep-ort-status buffer "COMP")
		(flymake-clear-buildfile-cache)
		(flymake-clear-project-include-dirs-cache)

		(flymake-set-buffer-check-was-interrupted buffer nil)
		(flymake-set-buffer-data buffer (flymake-makehash 'equal))

		(let* ((source-file-name  (buffer-file-name buffer))
		       (init-f (flymake-get-init-function source-file-name))
		       (cleanup-f (flymake-get-cleanup-function source-file-name))
		       (cmd-and-args (funcall init-f buffer))
		       (cmd          (nth 0 cmd-and-args))
		       (args         (nth 1 cmd-and-args))
					   (dir          (nth 2 cmd-and-args)))
		    (if (not cmd-and-args)
			(progn
			    (flymake-log 0 "init function %s for %s failed, cleaning up" init-f source-file-name)
			    (funcall cleanup-f buffer))
			(progn
			    (flymake-set-buffer-last-change-time buffer nil)
			    (flymake-start-syntax-check-process buffer cmd args dir))))))))

(defun flymake-start-syntax-check-process (buffer cmd args dir)
    "Start syntax check process."
    (let* ((process nil))
	(condition-case err
	    (progn
                (when dir
		    (let ((default-directory dir))
			  (flymake-log 3 "starting process on dir %s" default-directory)))
		(setq process (get-process (apply 'start-process "flymake-proc" nil cmd args)))
		(set-process-sentinel process 'flymake-process-sentinel)
		(set-process-filter process 'flymake-process-filter)

		(flymake-reg-names (process-id process) (buffer-name buffer))

		(flymake-set-buffer-is-running buffer t)
		(flymake-set-buffer-last-change-time buffer nil)
		(flymake-set-buffer-check-start-time buffer (flymake-float-time))

		(flymake-report-status buffer nil "*")
		(flymake-log 2 "started process %d, command=%s, dir=%s"
			   (process-id process) (process-command process) default-directory)
		process)
	    (error
		(let* ((err-str (format "Failed to launch syntax check process '%s' with args %s: %s"
			     cmd args (error-message-string err)))
		      (source-file-name (buffer-file-name buffer))
		      (cleanup-f        (flymake-get-cleanup-function source-file-name)))
		    (flymake-log 0 err-str)
		    (funcall cleanup-f buffer)
		    (flymake-report-fatal-status buffer "PROCERR" err-str))))))

(defun flymake-kill-process (pid &optional rest)
    "Kill process PID."
    (signal-process pid 9)
    (let* ((buffer-name (flymake-get-source-buffer-name pid)))
	(when (and buffer-name (get-buffer buffer-name))
	    (flymake-set-buffer-check-was-interrupted (get-buffer buffer-name) t)))
    (flymake-log 1 "killed process %d" pid))

(defun flymake-stop-all-syntax-checks ()
    "Kill all syntax check processes."
    (interactive)
    (let ((pids  (copy-hash-table flymake-pid-to-names)))
	(maphash 'flymake-kill-process pids)))

(defun flymake-compilation-is-running ()
   (and (boundp 'compilation-in-progress)
	compilation-in-progress))

(defun flymake-compile ()
    "Kill all flymake syntax checks, start compilation."
    (interactive)
    (flymake-stop-all-syntax-checks)
    (call-interactively 'compile))

(defvar flymake-is-running nil
  "If t, flymake syntax check process is running for the current buffer")

(make-variable-buffer-local 'flymake-is-running)

(defun flymake-get-buffer-is-running (buffer)
    (flymake-get-buffer-var buffer 'flymake-is-running))

(defun flymake-set-buffer-is-running (buffer is-running)
    (flymake-set-buffer-var buffer 'flymake-is-running is-running))

(defvar flymake-timer nil
  "Timer for starting syntax check.")

(make-variable-buffer-local 'flymake-timer)

(defun flymake-get-buffer-timer (buffer)
    (flymake-get-buffer-var buffer 'flymake-timer))

(defun flymake-set-buffer-timer (buffer timer)
    (flymake-set-buffer-var buffer 'flymake-timer timer))

(defvar flymake-last-change-time nil
  "Time of last buffer change.")

(make-variable-buffer-local 'flymake-last-change-time)

(defun flymake-get-buffer-last-change-time (buffer)
    (flymake-get-buffer-var buffer 'flymake-last-change-time))

(defun flymake-set-buffer-last-change-time (buffer change-time)
    (flymake-set-buffer-var buffer 'flymake-last-change-time change-time))

(defvar flymake-check-start-time nil
  "Time at which syntax check was started.")

(make-variable-buffer-local 'flymake-check-start-time)

(defun flymake-get-buffer-check-start-time (buffer)
    (flymake-get-buffer-var buffer 'flymake-check-start-time))

(defun flymake-set-buffer-check-start-time (buffer check-start-time)
    (flymake-set-buffer-var buffer 'flymake-check-start-time check-start-time))

(defvar flymake-check-was-interrupted nil
  "t if syntax check was killed by flymake-compile")

(make-variable-buffer-local 'flymake-check-was-interrupted)

(defun flymake-get-buffer-check-was-interrupted (buffer)
    (flymake-get-buffer-var buffer 'flymake-check-was-interrupted))

(defun flymake-set-buffer-check-was-interrupted (buffer interrupted)
    (flymake-set-buffer-var buffer 'flymake-check-was-interrupted interrupted))

(defcustom flymake-no-changes-timeout 0.5
    "Time to wait after last change before starting compilation."
    :group 'flymake
    :type 'number)

(defun flymake-on-timer-event (buffer)
    "Start a syntax check for buffer BUFFER if necessary."
    ;+(flymake-log 3 "timer: running=%s, time=%s, cur-time=%s" (flymake-get-buffer-is-running buffer) (flymake-get-buffer-last-change-time buffer) (flymake-float-time))
     (when (and (bufferp buffer) (not (flymake-get-buffer-is-running buffer)))
	(save-excursion
	    (set-buffer buffer)
	    (when (and (flymake-get-buffer-last-change-time buffer)
		       (> (flymake-float-time) (+ flymake-no-changes-timeout (flymake-get-buffer-last-change-time buffer))))
		(flymake-set-buffer-last-change-time buffer nil)
		(flymake-log 3 "starting syntax check as more than 1 second passed since last change")
		(flymake-start-syntax-check buffer)))))

(defun flymake-start-syntax-check-for-current-buffer ()
    "Run 'flymake-start-syntax-check' for current buffer if it isn't already running."
    (interactive)
    (flymake-start-syntax-check (current-buffer)))

(defun flymake-current-line-no ()
    "Return number of current line in current buffer."
    (interactive)
    (let ((beg  (point-min))
	  (end  (if (= (point) (point-max)) (point) (1+ (point)))))
	(count-lines beg end)))

(defun flymake-get-line-count (buffer)
    "Return number of lines in buffer BUFFER."
    (unless (bufferp buffer)
	(error "Invalid buffer"))
    (save-excursion
	(set-buffer buffer)
	(count-lines (point-min) (point-max))))

(defun flymake-count-lines (buffer)
    "Return number of lines in buffer BUFFER."
    (save-excursion
	(set-buffer buffer)
	(count-lines (point-min) (point-max))))

(defun flymake-get-point-pixel-pos ()
    "Return point position in pixels: (x, y)."
    (let ((mouse-pos  (mouse-position))
	  (pixel-pos  nil)
	  (ret        nil))
	(if (car (cdr mouse-pos))
	    (progn
		(set-mouse-position (flymake-selected-frame) (current-column) (flymake-current-row))
		(setq pixel-pos (mouse-pixel-position))
		(set-mouse-position (car mouse-pos) (car (cdr mouse-pos)) (cdr (cdr mouse-pos)))
		(setq ret (list (car (cdr pixel-pos)) (cdr (cdr pixel-pos)))))
	    (progn
		(setq ret '(0 0))))
	(flymake-log 3 "mouse pos is %s" ret)
	ret))

(defun flymake-display-err-menu-for-current-line ()
   "Display a menu with errors/warnings for current line if it has errors and/or warnings."
   (interactive)
   (let* ((line-no             (flymake-current-line-no))
	  (line-err-info-list  (nth 0 (flymake-find-err-info (flymake-get-buffer-err-info (current-buffer)) line-no)))
	  (menu-data           (flymake-make-err-menu-data line-no line-err-info-list))
	  (choice              nil)
	  (mouse-pos           (flymake-get-point-pixel-pos))
	  (moved-mouse-pos     (list (car mouse-pos) (+ 10 (car (cdr mouse-pos)))))
	  (menu-pos            (list (flymake-get-point-pixel-pos) (selected-window))))
       (if menu-data
	   (progn
	       (setq choice (flymake-popup-menu menu-pos menu-data))
			   (flymake-log 3 "choice=%s" choice)
	       (when choice
			       (eval choice)))
	   (flymake-log 1 "no errors for line %d" line-no))))

(defun flymake-make-err-menu-data (line-no line-err-info-list)
   "Make a (menu-title (item-title item-action)*) list with errors/warnings from line-err-info."
   (let* ((menu-items  nil))
       (when line-err-info-list
	   (let* ((count           (length line-err-info-list))
		  (menu-item-text  nil))
	       (while (> count 0)
		    (setq menu-item-text (flymake-ler-get-text (nth (1- count) line-err-info-list)))
		    (let* ((file       (flymake-ler-get-file (nth (1- count) line-err-info-list)))
			   (full-file  (flymake-ler-get-full-file (nth (1- count) line-err-info-list)))
			   (line       (flymake-ler-get-line (nth (1- count) line-err-info-list))))
			(if file
			    (setq menu-item-text (concat menu-item-text " - " file "(" (format "%d" line) ")")))
			(setq menu-items (cons (list menu-item-text
						     (if file (list 'flymake-goto-file-and-line full-file line) nil))
					       menu-items)))
		    (setq count (1- count)))
	       (flymake-log 3 "created menu-items with %d item(s)" (length menu-items))))
       (if menu-items
	   (let* ((menu-title  (format "Line %d: %d error(s), %d warning(s)" line-no
				      (flymake-get-line-err-count line-err-info-list "e")
				      (flymake-get-line-err-count line-err-info-list "w"))))
	       (list menu-title menu-items))
	   nil)))

(defun flymake-goto-file-and-line (file line)
    "Try to get buffer for file and goto line line in it"
    (if (not (file-exists-p file))
	(flymake-log 1 "file %s does not exists" file)
	(progn
	    (find-file file)
	    (goto-line line))))

;; flymake minor mode declarations
(defvar flymake-mode nil)

(make-variable-buffer-local 'flymake-mode)

(defvar flymake-mode-line nil
  "")

(make-variable-buffer-local 'flymake-mode-line)

(defun flymake-get-buffer-mode-line (buffer)
    (flymake-get-buffer-var buffer 'flymake-mode-line))

(defun flymake-set-buffer-mode-line (buffer mode-line-string)
    (flymake-set-buffer-var buffer 'flymake-mode-line mode-line-string))

(defvar flymake-mode-line-e-w nil)

(make-variable-buffer-local 'flymake-mode-line-e-w)

(defun flymake-get-buffer-mode-line-e-w (buffer)
    (flymake-get-buffer-var buffer 'flymake-mode-line-e-w))

(defun flymake-set-buffer-mode-line-e-w (buffer e-w)
    (flymake-set-buffer-var buffer 'flymake-mode-line-e-w e-w))

(defvar flymake-mode-line-status nil)

(make-variable-buffer-local 'flymake-mode-line-status)

(defun flymake-get-buffer-mode-line-status (buffer)
    (flymake-get-buffer-var buffer 'flymake-mode-line-status))

(defun flymake-set-buffer-mode-line-status (buffer status)
    (flymake-set-buffer-var buffer 'flymake-mode-line-status status))

(defun flymake-report-status (buffer e-w &optional status)
    "Show status in mode line."
    (when (bufferp buffer)
	(save-excursion
	    (set-buffer buffer)
	    (when e-w
		(flymake-set-buffer-mode-line-e-w buffer e-w)
	    )
	    (when status
		(flymake-set-buffer-mode-line-status buffer status))
	    (let* ((mode-line " Flymake"))
		(when (> (length (flymake-get-buffer-mode-line-e-w buffer)) 0)
		    (setq mode-line (concat mode-line ":"  (flymake-get-buffer-mode-line-e-w buffer))))
		(setq mode-line (concat mode-line (flymake-get-buffer-mode-line-status buffer)))
		(flymake-set-buffer-mode-line buffer mode-line)
		(force-mode-line-update)))))

(defun flymake-display-warning (warning)
    "Display a warning to user."
    (message-box warning))

(defcustom flymake-gui-warnings-enabled t
    "Enables/disables gui warnings."
	:group 'flymake
	:type 'boolean)

(defun flymake-report-fatal-status (buffer status warning)
    "Display a warning and switch flymake mode off."
	(when flymake-gui-warnings-enabled
	(flymake-display-warning (format "Flymake: %s. Flymake will be switched OFF" warning))
	)
    (save-excursion
	(set-buffer buffer)
	(flymake-mode 0)
	(flymake-log 0 "switched OFF Flymake mode for buffer %s due to fatal status %s, warning %s"
		     (buffer-name buffer) status warning)))

(defun flymake-mode (&optional arg)
    "Toggle flymake mode on/off."
    (interactive)
    (let ((old-flymake-mode flymake-mode)
		  (turn-on nil))

	(setq turn-on
	    (if (null arg)
		(not flymake-mode)
	    ;else
		(> (prefix-numeric-value arg) 0)))

	(if turn-on
	    (if (flymake-can-syntax-check-file (buffer-file-name))
		(flymake-mode-on)
		(flymake-log 2 "flymake cannot check syntax in buffer %s" (buffer-name)))
	    (flymake-mode-off))
	(force-mode-line-update)))

(defcustom flymake-start-syntax-check-on-find-file t
    "Start syntax check on find file."
    :group 'flymake
    :type 'boolean)

;;;###autoload
(unless (assq 'flymake-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(flymake-mode flymake-mode-line) minor-mode-alist)))

;;;###autoload
(defun flymake-mode-on ()
    "Turn flymake mode on."
    (when (not flymake-mode)
	(make-local-variable 'after-change-functions)
	(setq after-change-functions (cons 'flymake-after-change-function after-change-functions))
	(add-hook 'after-save-hook 'flymake-after-save-hook)
	(add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook)
	;+(add-hook 'find-file-hooks 'flymake-find-file-hook)

	(flymake-report-status (current-buffer) "" "")

	(flymake-set-buffer-timer (current-buffer) (run-at-time nil 1 'flymake-on-timer-event (current-buffer)))

	(setq flymake-mode t)
	(flymake-log 1 "flymake mode turned ON for buffer %s" (buffer-name (current-buffer)))
	(when flymake-start-syntax-check-on-find-file
	    (flymake-start-syntax-check-for-current-buffer)))) ; will be started by on-load hook

;;;###autoload
(defun flymake-mode-off ()
    "Turn flymake mode off."
    (when flymake-mode
	(setq after-change-functions (delq 'flymake-after-change-function  after-change-functions))
	(remove-hook 'after-save-hook (function flymake-after-save-hook) t)
	(remove-hook 'kill-buffer-hook (function flymake-kill-buffer-hook) t)
	;+(remove-hook 'find-file-hooks (function flymake-find-file-hook) t)

	(flymake-delete-own-overlays (current-buffer))

	(when (flymake-get-buffer-timer (current-buffer))
	    (cancel-timer (flymake-get-buffer-timer (current-buffer)))
	    (flymake-set-buffer-timer (current-buffer) nil))

	(flymake-set-buffer-is-running (current-buffer) nil)

	(setq flymake-mode nil)
	(flymake-log 1 "flymake mode turned OFF for buffer %s" (buffer-name (current-buffer)))))

(defcustom flymake-start-syntax-check-on-newline t
    "Start syntax check if newline char was added/removed from the buffer."
    :group 'flymake
    :type 'boolean)

(defun flymake-after-change-function (start stop len)
    "Start syntax check for current buffer if it isn't already running"
    ;+(flymake-log 0 "setting change time to %s" (flymake-float-time))
    (let((new-text (buffer-substring start stop)))
	(when (and flymake-start-syntax-check-on-newline (equal new-text "\n"))
	    (flymake-log 3 "starting syntax check as new-line has been seen")
	    (flymake-start-syntax-check-for-current-buffer))
	(flymake-set-buffer-last-change-time (current-buffer) (flymake-float-time))))

(defun flymake-after-save-hook ()
    (if (local-variable-p 'flymake-mode (current-buffer)) ; (???) other way to determine whether flymake is active in buffer being saved?
	(progn
	    (flymake-log 3 "starting syntax check as buffer was saved")
	    (flymake-start-syntax-check-for-current-buffer)))) ; no more mode 3. cannot start check if mode 3 (to temp copies) is active - (???)

(defun flymake-kill-buffer-hook ()
    (when (flymake-get-buffer-timer (current-buffer))
	(cancel-timer (flymake-get-buffer-timer (current-buffer)))
	(flymake-set-buffer-timer (current-buffer) nil)))

(defun flymake-find-file-hook ()
    ;+(when flymake-start-syntax-check-on-find-file
    ;+    (flymake-log 3 "starting syntax check on file open")
    ;+    (flymake-start-syntax-check-for-current-buffer)
    ;+)
    (when (and (not (local-variable-p 'flymake-mode (current-buffer)))
	   (flymake-can-syntax-check-file (buffer-file-name (current-buffer))))
	(flymake-mode)
	(flymake-log 3 "automatically turned ON flymake mode")))

(defun flymake-get-first-err-line-no (err-info-list)
    "Return first line with error."
    (when err-info-list
	(flymake-er-get-line (car err-info-list))))

(defun flymake-get-last-err-line-no (err-info-list)
    "Return last line with error."
    (when err-info-list
	(flymake-er-get-line (nth (1- (length err-info-list)) err-info-list))))

(defun flymake-get-next-err-line-no (err-info-list line-no)
    "Return next line with error."
    (when err-info-list
	(let* ((count  (length err-info-list))
	       (idx    0))
	    (while (and (< idx count) (>= line-no (flymake-er-get-line (nth idx err-info-list))))
		(setq idx (1+ idx)))
	    (if (< idx count)
		(flymake-er-get-line (nth idx err-info-list))))))

(defun flymake-get-prev-err-line-no (err-info-list line-no)
    "Return prev line with error."
    (when err-info-list
	(let* ((count (length err-info-list)))
	    (while (and (> count 0) (<= line-no (flymake-er-get-line (nth (1- count) err-info-list))))
		(setq count (1- count)))
	    (if (> count 0)
		(flymake-er-get-line (nth (1- count) err-info-list))))))

(defun flymake-skip-whitespace ()
    "Move forward until non-whitespace is reached."
    (while (looking-at "[ \t]")
	(forward-char)))

(defun flymake-goto-line (line-no)
    "goto-line, then skip whitespace"
    (goto-line line-no)
    (flymake-skip-whitespace))

(defun flymake-goto-next-error ()
    "go to next error in err ring"
    (interactive)
    (let ((line-no (flymake-get-next-err-line-no (flymake-get-buffer-err-info (current-buffer)) (flymake-current-line-no))))
	(when (not line-no)
	    (setq line-no (flymake-get-first-err-line-no (flymake-get-buffer-err-info (current-buffer))))
	    (flymake-log 1 "passed end of file"))
	(if line-no
	    (flymake-goto-line line-no)
	    (flymake-log 1 "no errors in current buffer"))))

(defun flymake-goto-prev-error ()
    "go to prev error in err ring"
    (interactive)
    (let ((line-no (flymake-get-prev-err-line-no (flymake-get-buffer-err-info (current-buffer)) (flymake-current-line-no))))
	(when (not line-no)
	    (setq line-no (flymake-get-last-err-line-no (flymake-get-buffer-err-info (current-buffer))))
	    (flymake-log 1 "passed beginning of file"))
	(if line-no
	    (flymake-goto-line line-no)
	    (flymake-log 1 "no errors in current buffer"))))

(defun flymake-patch-err-text (string)
    (if (string-match "^[\n\t :0-9]*\\(.*\\)$" string)
	(match-string 1 string)
	string))

;;;; general init-cleanup and helper routines
(defun flymake-create-temp-inplace (file-name prefix)
    (unless (stringp file-name)
	(error "Invalid file-name"))
    (or prefix
	(setq prefix "flymake"))
    (let* ((temp-name   (concat (file-name-sans-extension file-name)
				"_" prefix
				(and (file-name-extension file-name)
				     (concat "." (file-name-extension file-name))))))
	(flymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
	temp-name))

(defun flymake-create-temp-with-folder-structure (file-name prefix)
    (unless (stringp file-name)
	(error "Invalid file-name"))

    (let* ((dir       (file-name-directory file-name))
		   (slash-pos (string-match "/" dir))
		   (temp-dir  (concat (flymake-ensure-ends-with-slash (flymake-get-temp-dir)) (substring dir (1+ slash-pos)))))

	    (file-truename (concat (flymake-ensure-ends-with-slash temp-dir)
				(file-name-nondirectory file-name)))))

(defun flymake-strrchr (str ch)
    (let* ((count  (length str))
	   (pos    nil))
	(while (and (not pos) (> count 0))
	    (if (= ch (elt str (1- count)))
		(setq pos (1- count)))
	    (setq count (1- count)))
	pos))

(defun flymake-delete-temp-directory (dir-name)
    "attempt to delete temp dir created by flymake-create-temp-with-folder-structure, do not fail on error."
    (let* ((temp-dir    (flymake-get-temp-dir))
	   (suffix      (substring dir-name (1+ (length temp-dir))))
	   (slash-pos   nil))

	(while (> (length suffix) 0)
	    ;+(flymake-log 0 "suffix=%s" suffix)
			(flymake-safe-delete-directory (file-truename (concat (flymake-ensure-ends-with-slash temp-dir) suffix)))
	    (setq slash-pos (flymake-strrchr suffix (string-to-char "/")))
	    (if slash-pos
		(setq suffix (substring suffix 0 slash-pos))
		(setq suffix "")))))

(defun flymake-init-create-temp-buffer-copy (buffer create-temp-f)
    "Make a temporary copy of the current buffer, save its name in buffer data and return the name."
    (let*  ((source-file-name       (buffer-file-name buffer))
	    (temp-source-file-name  (funcall create-temp-f source-file-name "flymake")))

	(flymake-save-buffer-in-file buffer temp-source-file-name)
	(flymake-set-buffer-value buffer "temp-source-file-name" temp-source-file-name)
	temp-source-file-name))

(defun flymake-simple-cleanup (buffer)
    "Do cleanup after 'flymake-init-create-temp-buffer-copy'.
Delete temp file."
    (let* ((temp-source-file-name (flymake-get-buffer-value buffer "temp-source-file-name")))
	(flymake-safe-delete-file temp-source-file-name)
	(flymake-set-buffer-last-change-time buffer nil)))

(defun flymake-get-real-file-name (buffer file-name-from-err-msg)
    "Translate file name from error message to `real' file name. 
Return full-name. Names are real, not patched."
    (let* ((real-name              nil)
	   (source-file-name       (buffer-file-name buffer))
	   (master-file-name       (flymake-get-buffer-value buffer "master-file-name"))
	   (temp-source-file-name  (flymake-get-buffer-value buffer "temp-source-file-name"))
	   (temp-master-file-name  (flymake-get-buffer-value buffer "temp-master-file-name"))
	   (base-dirs              (list (flymake-get-buffer-value buffer "base-dir")
					 (file-name-directory source-file-name)
					 (if master-file-name (file-name-directory master-file-name) nil)))
	   (files                  (list (list source-file-name       source-file-name)
					 (list temp-source-file-name  source-file-name)
					 (list master-file-name       master-file-name)
					 (list temp-master-file-name  master-file-name))))

	(when (equal 0 (length file-name-from-err-msg))
	    (setq file-name-from-err-msg source-file-name))

	(setq real-name (flymake-get-full-patched-file-name file-name-from-err-msg base-dirs files))
	; if real-name is nil, than file name from err msg is none of the files we've patched
	(if (not real-name)
	    (setq real-name (flymake-get-full-nonpatched-file-name file-name-from-err-msg base-dirs)))
	(if (not real-name)
	    (setq real-name file-name-from-err-msg))
	(setq real-name (flymake-fix-path-name real-name))
	(flymake-log 3 "get-real-file-name: file-name=%s real-name=%s" file-name-from-err-msg real-name)
	real-name))

(defun flymake-get-full-patched-file-name (file-name-from-err-msg base-dirs files)
    (let* ((base-dirs-count  (length base-dirs))
	   (file-count       (length files))
	   (real-name        nil))

	(while (and (not real-name) (> base-dirs-count 0))
	    (setq file-count (length files))
	    (while (and (not real-name) (> file-count 0))
		(let* ((this-dir        (nth (1- base-dirs-count) base-dirs))
		       (this-file       (nth 0 (nth (1- file-count) files)))
		       (this-real-name  (nth 1 (nth (1- file-count) files))))
		    ;+(flymake-log 0 "this-dir=%s this-file=%s this-real=%s msg-file=%s" this-dir this-file this-real-name file-name-from-err-msg)
		    (when (and this-dir this-file (flymake-same-files
						   (flymake-get-absolute-file-name-basedir file-name-from-err-msg this-dir)
						   this-file))
			(setq real-name this-real-name)))
		(setq file-count (1- file-count)))
	    (setq base-dirs-count (1- base-dirs-count)))
	real-name))

(defun flymake-get-full-nonpatched-file-name (file-name-from-err-msg base-dirs)
    (let* ((real-name  nil))
	(if (file-name-absolute-p file-name-from-err-msg)
	    (setq real-name file-name-from-err-msg)
	    (let* ((base-dirs-count  (length base-dirs)))
		(while (and (not real-name) (> base-dirs-count 0))
		    (let* ((full-name (flymake-get-absolute-file-name-basedir file-name-from-err-msg
									      (nth (1- base-dirs-count) base-dirs))))
			(if (file-exists-p full-name)
			    (setq real-name full-name))
			(setq base-dirs-count (1- base-dirs-count))))))
	real-name))

(defun flymake-get-absolute-file-name-basedir (file-name dir-name)
    (if (file-name-absolute-p file-name)
	file-name
	(concat dir-name "/" file-name)))

(defun flymake-init-find-buildfile-dir (buffer source-file-name buildfile-name)
    "Find buildfile, store its dir in buffer data and return its dir, if found."
    (let* ((buildfile-dir  (flymake-find-buildfile buildfile-name
						   (file-name-directory source-file-name)
						   flymake-buildfile-dirs)))
	(if (not buildfile-dir)
	    (progn
		(flymake-log 1 "no buildfile (%s) for %s" buildfile-name source-file-name)
		(flymake-report-fatal-status buffer "NOMK" (format "No buildfile (%s) found for %s" buildfile-name source-file-name))
	    )
	    (progn
		(flymake-set-buffer-value buffer "base-dir" buildfile-dir)))
	buildfile-dir))

(defun flymake-init-create-temp-source-and-master-buffer-copy (buffer get-incl-dirs-f create-temp-f master-file-masks include-regexp-list)
    "Find master file (or buffer), create it's copy along with a copy of the source file."
    (let* ((source-file-name       (buffer-file-name buffer))
	   (temp-source-file-name  (flymake-init-create-temp-buffer-copy buffer create-temp-f))
	   (master-file-name       nil)
	   (temp-master-file-name  nil)
	   (master-and-temp-master (flymake-create-master-file
				    source-file-name temp-source-file-name
				    get-incl-dirs-f create-temp-f
				    master-file-masks include-regexp-list)))

	(if (not master-and-temp-master)
	    (progn
		(flymake-log 1 "cannot find master file for %s" source-file-name)
		(flymake-report-status buffer "!" "") ; NOMASTER
	    )
	    (progn
		(setq master-file-name       (nth 0 master-and-temp-master))
		(setq temp-master-file-name  (nth 1 master-and-temp-master))
		(flymake-set-buffer-value buffer "master-file-name"      master-file-name)
		(flymake-set-buffer-value buffer "temp-master-file-name" temp-master-file-name)
	    ))
	temp-master-file-name))

(defun flymake-master-cleanup (buffer)
    (flymake-simple-cleanup buffer)
    (flymake-safe-delete-file (flymake-get-buffer-value buffer "temp-master-file-name")))

;;;; make-specific init-cleanup routines
(defun flymake-get-syntax-check-program-args (source-file-name base-dir use-relative-base-dir use-relative-source get-cmd-line-f)
    "Create a command line for syntax check using GET-CMD-LINE-F."
    (let* ((my-base-dir  base-dir)
	   (my-source    source-file-name))

	(when use-relative-base-dir
	    (setq my-base-dir (flymake-build-relative-path (file-name-directory source-file-name) base-dir)))

	(when use-relative-source
	    (setq my-source (concat (flymake-build-relative-path base-dir (file-name-directory source-file-name))
				    (file-name-nondirectory source-file-name))))
	(funcall get-cmd-line-f my-source my-base-dir)))

(defun flymake-get-make-cmdline (source base-dir)
    (list "make"
	  (list "-s"
			"-C"
			base-dir
			(concat "CHK_SOURCES=" source)
			"SYNTAX_CHECK_MODE=1"
			"check-syntax")))

(defun flymake-get-ant-cmdline (source base-dir)
    (list "ant"
	  (list "-buildfile"
		(concat base-dir "/" "build.xml")
		(concat "-DCHK_SOURCES=" source)
		"check-syntax")))

(defun flymake-simple-make-init-impl (buffer create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
    "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
    (let* ((args nil)
	   (source-file-name   (buffer-file-name buffer))
	   (buildfile-dir      (flymake-init-find-buildfile-dir buffer source-file-name build-file-name)))
	(if buildfile-dir
	    (let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy buffer create-temp-f)))
		(setq args (flymake-get-syntax-check-program-args temp-source-file-name buildfile-dir
							       use-relative-base-dir use-relative-source
							       get-cmdline-f))))
	args))

(defun flymake-simple-make-init (buffer)
    (flymake-simple-make-init-impl buffer 'flymake-create-temp-inplace t t "Makefile" 'flymake-get-make-cmdline))

(defun flymake-master-make-init (buffer get-incl-dirs-f master-file-masks include-regexp-list)
    "create make command line for a source file checked via master file compilation"
    (let* ((make-args nil)
	   (temp-master-file-name (flymake-init-create-temp-source-and-master-buffer-copy
				   buffer get-incl-dirs-f 'flymake-create-temp-inplace
				   master-file-masks include-regexp-list)))
	(when temp-master-file-name
	    (let* ((buildfile-dir (flymake-init-find-buildfile-dir buffer temp-master-file-name "Makefile")))
		(if  buildfile-dir
				    (setq make-args (flymake-get-syntax-check-program-args
									 temp-master-file-name buildfile-dir nil nil 'flymake-get-make-cmdline)))))
		make-args))

(defun flymake-find-make-buildfile (source-dir)
    (flymake-find-buildfile "Makefile" source-dir flymake-buildfile-dirs))

;;;; .h/make specific
(defun flymake-master-make-header-init (buffer)
    (flymake-master-make-init buffer
			      'flymake-get-include-dirs
			      '(".+\\.cpp$" ".+\\.c$")
			      '("[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\"" 1 2)))

;;;; .java/make specific
(defun flymake-simple-make-java-init (buffer)
    (flymake-simple-make-init-impl buffer 'flymake-create-temp-with-folder-structure nil nil "Makefile" 'flymake-get-make-cmdline))

(defun flymake-simple-ant-java-init (buffer)
    (flymake-simple-make-init-impl buffer 'flymake-create-temp-with-folder-structure nil nil "build.xml" 'flymake-get-ant-cmdline))

(defun flymake-simple-java-cleanup (buffer)
    "cleanup after flymake-simple-make-java-init -- delete temp file and dirs"
    (let* ((temp-source-file-name (flymake-get-buffer-value buffer "temp-source-file-name")))
	(flymake-safe-delete-file temp-source-file-name)
	(when temp-source-file-name
	    (flymake-delete-temp-directory (file-name-directory temp-source-file-name)))))

;;;; perl-specific init-cleanup routines
(defun flymake-perl-init (buffer)
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy buffer 'flymake-create-temp-inplace))
		   (local-file  (concat (flymake-build-relative-path (file-name-directory (buffer-file-name (current-buffer)))
															 (file-name-directory temp-file))
								(file-name-nondirectory temp-file))))
	(list "perl" (list "-wc " local-file))))

;;;; tex-specific init-cleanup routines
(defun flymake-get-tex-args (file-name)
    ;(list "latex" (list "-c-style-errors" file-name))
    (list "texify" (list "--pdf" "--tex-option=-c-style-errors" file-name)))

(defun flymake-simple-tex-init (buffer)
    (flymake-get-tex-args (flymake-init-create-temp-buffer-copy buffer 'flymake-create-temp-inplace)))

(defun flymake-master-tex-init (buffer)
    (let* ((temp-master-file-name (flymake-init-create-temp-source-and-master-buffer-copy
				   buffer 'flymake-get-include-dirs-dot 'flymake-create-temp-inplace
				   '(".+\\.tex$")
				   '("[ \t]*\\input[ \t]*{\\(.*\\)\\(%s\\)}" 1 2))))
	(when temp-master-file-name
	    (flymake-get-tex-args temp-master-file-name))))

(defun flymake-get-include-dirs-dot (base-dir)
    '("."))

;;;; xml-specific init-cleanup routines
(defun flymake-xml-init(buffer)
    (list "xml" (list "val" (flymake-init-create-temp-buffer-copy buffer 'flymake-create-temp-inplace))))

(provide 'flymake)

;;; arch-tag: 8f0d6090-061d-4cac-8862-7c151c4a02dd
;;; flymake.el ends here
