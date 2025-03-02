;; -*- lexical-binding:t -*-
(defun dabbrev-expand (arg)
  "Expand previous word \"dynamically\".

Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are
considered.  If still no suitable word is found, then look in the
buffers accepted by the function pointed out by variable
`dabbrev-friend-buffer-function', if `dabbrev-check-other-buffers'
says so.  Then, if `dabbrev-check-all-buffers' is non-nil, look in
all the other buffers, subject to constraints specified
by `dabbrev-ignored-buffer-names' and `dabbrev-ignored-buffer-regexps'.

A positive prefix argument, N, says to take the Nth backward *distinct*
possibility.  A negative argument says search forward.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried.

The variable `dabbrev-backward-only' may be used to limit the
direction of search to backward if set non-nil.

See also `dabbrev-abbrev-char-regexp' and \\[dabbrev-completion]."
  (interactive "*P")
  (let (abbrev record-case-pattern
	       expansion old direction (orig-point (point)))
    ;; abbrev -- the abbrev to expand
    ;; expansion -- the expansion found (eventually) or nil until then
    ;; old -- the text currently in the buffer
    ;;    (the abbrev, or the previously-made expansion)
    (save-excursion
      (if (and (null arg)
	       (markerp dabbrev--last-abbrev-location)
	       (marker-position dabbrev--last-abbrev-location)
	       (or (eq last-command this-command)
		   (and (window-minibuffer-p)
			(= dabbrev--last-abbrev-location
			   (point)))))
	  ;; Find a different expansion for the same abbrev as last time.
	  (progn
	    (setq abbrev dabbrev--last-abbreviation)
	    (setq old dabbrev--last-expansion)
	    (setq direction dabbrev--last-direction))
	;; If the user inserts a space after expanding
	;; and then asks to expand again, always fetch the next word.
	(if (and (eq (preceding-char) ?\s)
		 (markerp dabbrev--last-abbrev-location)
		 (marker-position dabbrev--last-abbrev-location)
		 (= (point) (1+ dabbrev--last-abbrev-location)))
	    (progn
	      ;; The "abbrev" to expand is just the space.
	      (setq abbrev " ")
	      (save-excursion
		(save-restriction
		  (widen)
		  (if (buffer-live-p dabbrev--last-buffer)
		      (set-buffer dabbrev--last-buffer))
		  ;; Find the end of the last "expansion" word.
		  (if (or (eq dabbrev--last-direction 1)
			  (and (eq dabbrev--last-direction 0)
			       (< dabbrev--last-expansion-location (point))))
		      (setq dabbrev--last-expansion-location
			    (+ dabbrev--last-expansion-location
			       (length dabbrev--last-expansion))))
		  (goto-char dabbrev--last-expansion-location)
		  ;; Take the following word, with intermediate separators,
		  ;; as our expansion this time.
		  (re-search-forward
		   (concat "\\(?:" dabbrev--abbrev-char-regexp "\\)+"))
		  (setq expansion (buffer-substring-no-properties
				   dabbrev--last-expansion-location (point)))

		  ;; Record the end of this expansion, in case we repeat this.
		  (setq dabbrev--last-expansion-location (point))))
	      ;; Indicate that dabbrev--last-expansion-location is
	      ;; at the end of the expansion.
	      (setq dabbrev--last-direction -1))

	  ;; We have a different abbrev to expand.
	  (dabbrev--reset-global-variables)
	  (setq direction (if (null arg)
			      (if dabbrev-backward-only 1 0)
			    (prefix-numeric-value arg)))
	  (setq abbrev (dabbrev--abbrev-at-point))
	  (setq record-case-pattern t)
	  (setq old nil)))

      ;;--------------------------------
      ;; Find the expansion
      ;;--------------------------------
      (or expansion
	  (setq expansion
		(dabbrev--find-expansion
                 abbrev direction
                 (dabbrev--ignore-case-p abbrev)))))
    (cond
     ((not expansion)
      (dabbrev--reset-global-variables)
      (if old
	  (save-excursion
	    (setq buffer-undo-list (cons orig-point buffer-undo-list))
	    ;; Put back the original abbrev with its original case pattern.
	    (search-backward old)
	    (insert abbrev)
	    (delete-region (point) (+ (point) (length old)))))
      (user-error "No%s dynamic expansion for `%s' found"
                  (if old " further" "") abbrev))
     (t
      (if (not (or (eq dabbrev--last-buffer dabbrev--last-buffer-found)
		   (minibuffer-window-active-p (selected-window))))
	  (progn
            (when (buffer-name dabbrev--last-buffer)
	      (message "Expansion found in `%s'"
		       (buffer-name dabbrev--last-buffer)))
	    (setq dabbrev--last-buffer-found dabbrev--last-buffer))
	(message nil))
      (if (and (or (eq (current-buffer) dabbrev--last-buffer)
		   (null dabbrev--last-buffer)
                   (buffer-live-p dabbrev--last-buffer))
	       (numberp dabbrev--last-expansion-location)
	       (and (> dabbrev--last-expansion-location (point))))
	  (setq dabbrev--last-expansion-location
		(copy-marker dabbrev--last-expansion-location)))
      ;; Success: stick it in and return.
      (setq buffer-undo-list (cons orig-point buffer-undo-list))
      (setq expansion (dabbrev--substitute-expansion old abbrev expansion
                                                     record-case-pattern))

      ;; Save state for re-expand.
      (setq dabbrev--last-expansion expansion)
      (setq dabbrev--last-abbreviation abbrev)
      (setq dabbrev--last-abbrev-location (point-marker))))))
