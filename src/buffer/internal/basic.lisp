(in-package :lem/buffer/internal)

(defun same-line-p (point1 point2)
  "Return t if POINT1 and POINT are on the same line."
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (eq (point-line point1) (point-line point2)))

(defun first-line-p (point)
  "Return t if the POINT is the first line in the buffer."
  (same-line-p point
               (buffer-start-point (point-buffer point))))

(defun last-line-p (point)
  "Return t if the POINT is the last line in the buffer."
  (same-line-p point
               (buffer-end-point (point-buffer point))))

(defun start-line-p (point)
  "Return t if POINT is at the beginning of a line."
  (zerop (point-charpos point)))

(defun end-line-p (point)
  "Return t if POINT is at the end of a line."
  (= (point-charpos point)
     (line:line-length (point-line point))))

(defun start-buffer-p (point)
  "Return t if POINT is at the beginning of the buffer."
  (point<= point (buffer-start-point (point-buffer point))))

(defun end-buffer-p (point)
  "Return t if POINT is at the end of the buffer."
  (point<= (buffer-end-point (point-buffer point)) point))

(defun %move-to-position (point linum line charpos)
  (assert (line:line-alive-p line))
  (assert (<= 0 charpos))
  (without-interrupts
    (point-change-line point linum line)
    (setf (point-charpos point) (min (line:line-length line) charpos)))
  point)

(defun move-point (point new-point)
  "Move POINT to the NEW-POINT."
  (assert (eq (point-buffer point)
              (point-buffer new-point)))
  (%move-to-position point
                     (point-linum new-point)
                     (point-line new-point)
                     (point-charpos new-point)))

(defun line-start (point)
  "Move POINT to the beginning of the line."
  (setf (point-charpos point) 0)
  point)

(defun line-end (point)
  "Move POINT to the end of the line."
  (setf (point-charpos point)
        (line:line-length (point-line point)))
  point)

(defun buffer-start (point)
  "Move POINT to the beginning of the buffer."
  (move-point point (buffer-start-point (point-buffer point))))

(defun buffer-end (point)
  "Move POINT to the end of the buffer."
  (move-point point (buffer-end-point (point-buffer point))))

(defun line-offset (point n &optional (charpos 0))
  "If 'point' is a positive number, move the line down. If it is a negative number, move the line up and return the moved 'point'.
If there is no line at the 'n' destination, the position of 'point' is left as it is and NIL is returned.
'charpos' is the offset from the start of the line after the move."
  (cond
    ((plusp n)
     (do ((i n (1- i))
          (line (point-line point) (line:line-next line)))
         ((null line) nil)
       (when (zerop i)
         (%move-to-position point (+ (point-linum point) n)
                            line charpos)
         (return point))))
    ((minusp n)
     (do ((i n (1+ i))
          (line (point-line point) (line:line-previous line)))
         ((null line) nil)
       (when (zerop i)
         (%move-to-position point (+ (point-linum point) n)
                            line charpos)
         (return point))))
    (t
     (setf (point-charpos point)
           (if (< charpos 0)
               0
               (min charpos
                    (line:line-length (point-line point)))))
     point)))

(declaim (inline %character-offset))
(defun %character-offset (point n fn zero-fn)
  (cond ((zerop n) (when zero-fn (funcall zero-fn)))
        ((plusp n)
         (do ((line (point-line point) (line:line-next line))
              (linum (point-linum point) (1+ linum))
              (charpos (point-charpos point) 0))
             ((null line) nil)
           (let ((w (- (line:line-length line) charpos)))
             (when (<= n w)
               (return (funcall fn linum line (+ charpos n))))
             (decf n (1+ w)))))
        (t
         (setf n (- n))
         (do* ((line (point-line point) (line:line-previous line))
               (linum (point-linum point) (1- linum))
               (charpos (point-charpos point) (and line (line:line-length line))))
             ((null line) nil)
           (when (<= n charpos)
             (return (funcall fn linum line (- charpos n))))
           (decf n (1+ charpos))))))

(defun character-offset (point n)
  "If 'n' is a positive number, move it forward. If it is a negative number, move it backward. Return the moved 'point'.
If the 'n' character is beyond the buffer, the position of 'point' is left as it is and NIL is returned."
  (%character-offset point n
                     (lambda (linum line charpos)
                       (%move-to-position point linum line charpos)
                       point)
                     (lambda ()
                       point)))

(defun character-at (point &optional (offset 0))
  "Return the character at the offset.
Return NIL if the buffer is out of range."
  (%character-offset point offset
                     (lambda (linum line charpos)
                       (declare (ignore linum))
                       (line:line-char line charpos))
                     (lambda ()
                       (line:line-char (point-line point)
                                  (point-charpos point)))))

(defun line-string (point)
  "Return the string at POINT."
  (line:line-string (point-line point)))

(defun text-property-at (point prop &optional (offset 0))
  "Return the property of 'prop' at the offset position from 'point' to 'offset'."
  (%character-offset point offset
                     (lambda (linum line charpos)
                       (declare (ignore linum))
                       (line:line-search-property line prop charpos))
                     (lambda ()
                       (line:line-search-property (point-line point)
                                             prop
                                             (point-charpos point)))))

(defun put-text-property (start-point end-point prop value)
  "Set one property of the text from START-POINT to END-POINT.

The third and fourth arguments PROP and VALUE specify the property to add."
  (assert (eq (point-buffer start-point)
              (point-buffer end-point)))
  (%map-region start-point end-point
               (lambda (line start end)
                 (line:line-add-property line
                                    start
                                    (if (null end)
                                        (line:line-length line)
                                        end)
                                    prop
                                    value
                                    (null end)))))

(defun remove-text-property (start-point end-point prop)
  "Remove one property from text from START-POINT to END-POINT.

The third argument PROP is a property to remove."
  (assert (eq (point-buffer start-point)
              (point-buffer end-point)))
  (%map-region start-point end-point
               (lambda (line start end)
                 (line:line-remove-property line
                                       start
                                       (if (null end)
                                           (line:line-length line)
                                           end)
                                       prop))))

(defun next-single-property-change (point prop &optional limit-point)
  "Move the value of the text property 'prop' from 'point' to a different position.
Return the translated version of 'point'.
If the scan does not stop until the end of the buffer, or if 'limit-point' is exceeded, the scan is interrupted and NIL is returned."
  ;; 移動後の`point`を返します。
  ;; バッファの最後まで走査が止まらないか、`limit-point`を越えると走査を中断しNILを返します。"
  (let ((first-value (text-property-at point prop)))
    (with-point ((curr point))
      (loop
        (unless (character-offset curr 1)
          (return nil))
        (unless (eq first-value (text-property-at curr prop))
          (return (move-point point curr)))
        (when (and limit-point (point<= limit-point curr))
          (return nil))))))

(defun previous-single-property-change (point prop &optional limit-point)
  "Move the text property 'prop' from 'point' forward to a different position.
Return the translated version of 'point'.
If the scan does not stop to the first position of the buffer, or if 'limit-point' is exceeded, the scan is interrupted and NIL is returned."
  (let ((first-value (text-property-at point prop -1)))
    (with-point ((curr point))
      (loop
        (unless (eq first-value (text-property-at curr prop -1))
          (return (move-point point curr)))
        (unless (character-offset curr -1)
          (return nil))
        (when (and limit-point (point> limit-point curr))
          (return nil))))))

(defun insert-character (point char &optional (n 1))
  "Insert the character 'char' into 'point' 'n' times."
  (insert-string/point point (str:repeat n (string char)))
  t)

(defun insert-string (point string &rest plist)
  "Insert the string 'string' into 'point'.
If 'plist' is specified, the text property is set to the range where 'string' is inserted."
  (if (null plist)
      (insert-string/point point string)
      (with-point ((start-point point))
        (insert-string/point point string)
        (let ((end-point (character-offset (copy-point start-point :temporary)
                                           (length string))))
          (unless end-point
            ;; fallback
            (log:error "end-point is nil")
            (setf end-point (line-end (copy-point start-point :temporary))))
          (loop :for (k v) :on plist :by #'cddr
                :do (put-text-property start-point end-point k v)))))
  t)

(defun delete-character (point &optional (n 1))
  "Delete the 'n'th character from the 'point' and return the deleted string.
Return NIL if the end of the buffer has been reached before deleting 'n' characters."
  (when (minusp n)
    (unless (character-offset point n)
      (return-from delete-character nil))
    (setf n (- n)))
  (unless (end-buffer-p point)
    (let ((string (delete-char/point point n)))
      string)))

(defun erase-buffer (buffer)
  "Delete the entire contents of the buffer."
  (buffer-start (buffer-point buffer))
  (delete-char/point (buffer-point buffer)
                     (count-characters (buffer-start-point buffer)
                                       (buffer-end-point buffer))))


(defun region-beginning (buffer)
  "Return the integer value of point or mark, whichever is smaller."
  (point-min (buffer-point buffer)
             (buffer-mark buffer)))

(defun region-end (buffer)
  "Return the integer value of point or mark, whichever is larger."
  (point-max (buffer-point buffer)
             (buffer-mark buffer)))

(defun %map-region (start end function)
  (when (point< end start)
    (rotatef start end))
  (let ((start-line (point-line start))
        (end-line (point-line end)))
    (loop :for line := start-line :then (line:line-next line)
          :for firstp := (eq line start-line)
          :for lastp := (eq line end-line)
          :do (funcall function
                       line
                       (if firstp
                           (point-charpos start)
                           0)
                       (if lastp
                           (point-charpos end)
                           nil))
          :until lastp))
  (values))

(defun map-region (start end function)
  (%map-region start end
               (lambda (line start end)
                 (funcall function
                          (line:line-substring line :start start :end end)
                          (not (null end))))))

(defun points-to-string (start-point end-point)
  "Return a string between 'start-point' and 'end-point'."
  (assert (eq (point-buffer start-point)
              (point-buffer end-point)))
  (with-output-to-string (out)
    (map-region start-point end-point
                (lambda (string lastp)
                  (write-string string out)
                  (unless lastp
                    (write-char #\newline out))))))

(defun count-characters (start-point end-point)
  "Count characters between START-POINT and END-POINT."
  (let ((count 0))
    (map-region start-point
                end-point
                (lambda (string lastp)
                  (incf count (length string))
                  (unless lastp
                    (incf count))))
    count))

(defun delete-between-points (start-point end-point)
  "Delete contents between START-POINT and END-POINT."
  (assert (eq (point-buffer start-point)
              (point-buffer end-point)))
  (unless (point< start-point end-point)
    (rotatef start-point end-point))
  (delete-char/point start-point
                     (count-characters start-point end-point)))

(defun count-lines (start-point end-point)
  "Return number of lines between START-POINT and END-POINT."
  (assert (eq (point-buffer start-point)
              (point-buffer end-point)))
  (abs (- (point-linum start-point)
          (point-linum end-point))))

(defun apply-region-lines (start-point end-point function)
  "For each line of 'start-point' to 'end-point' apply 'function' which takes a point as an argument."
  (when (point< end-point start-point)
    (rotatef start-point end-point))
  (with-point ((start-point start-point :right-inserting)
               (end-point end-point :right-inserting)
               (point start-point))
    (loop :while (point<= start-point end-point)
          :do (funcall function (move-point point start-point))
              (unless (line-offset start-point 1)
                (return)))))

(defun line-number-at-point (point)
  "Return the line number at POINT in the current buffer."
  (point-linum point))

(defun point-column (point)
  "Return the horizontal position of POINT.  Beginning of lines is column 0."
  (string-width (line-string point)
                :start 0
                :end (point-charpos point)
                :tab-size (variable-value 'tab-width :default point)))

(defun move-to-column (point column &optional force)
  "Move POINT to COLUMN in the current line.

Optional second argument FORCE non-nil means if COLUMN is in the middle of a
tab character, either change it to spaces, or insert enough spaces before it
reach COLUMN (otherwise).  In addition, if FORCE is t, and the lines is too
short to reach COLUMN, add spaces/tabs to get there."
  (line-end point)
  (let ((cur-column (point-column point)))
    (cond ((< column cur-column)
           (setf (point-charpos point)
                 (wide-index (line-string point)
                             column
                             :tab-size (variable-value 'tab-width :default point)))
           point)
          (force
           (insert-character point #\space (- column cur-column))
           (line-end point))
          (t
           (line-end point)))))

(defun position-at-point (point)
  "Return the offset of 'point' from the beginning of the buffer."
  (let ((offset (point-charpos point)))
    (do ((line (line:line-previous (point-line point)) (line:line-previous line)))
        ((null line) (1+ offset))
      (incf offset (1+ (line:line-length line))))))

(defun move-to-position (point position)
  "Move 'point' to the offset of 'position' from the beginning of the buffer and return its position.
If 'position' is out of the buffer, 'point' does not move and returns NIL."
  (let ((line-number (line-number-at-point point))
        (charpos (point-charpos point)))
    (or (character-offset (buffer-start point) (1- position))
        (progn
          (move-to-line point line-number)
          (line-offset point 0 charpos)
          nil))))

(defun point-bytes (point)
  "Return the offset of 'point' from the beginning of the buffer in bytes."
  (with-point ((point point))
    (let ((nbytes 0))
      (incf nbytes
            (babel:string-size-in-octets (line-string point)
                                         :end (point-charpos point)))
      (loop
        (unless (line-offset point -1) (return))
        (incf nbytes (1+ (babel:string-size-in-octets (line-string point)))))
      nbytes)))

(defun move-to-bytes (point bytes)
  (buffer-start point)
  (loop
    (let ((size (1+ (babel:string-size-in-octets (line-string point)))))
      (when (<= bytes size)
        (loop :for i :from 0
              :do (decf bytes (babel:string-size-in-octets (string (character-at point i))))
                  (when (<= bytes 0)
                    (character-offset point i)
                    (return-from move-to-bytes point))))
      (decf bytes size)
      (unless (line-offset point 1) (return)))))

(defun move-to-line (point line-number)
  "Move 'point' to line number 'line-number' and return the position after the move.
If 'line-number' is out of the buffer, 'point' does not move and returns NIL."
  (let ((cur-linum (line-number-at-point point))
        (nlines (buffer-nlines (point-buffer point))))
    (cond ((or (> 1 line-number)
               (< nlines line-number))
           nil)
          ((= line-number cur-linum)
           point)
          ((< line-number cur-linum)
           (if (< line-number (- cur-linum line-number))
               (line-offset (buffer-start point) (1- line-number))
               (line-offset point (- line-number cur-linum))))
          (t
           (if (< (- line-number cur-linum) (- nlines line-number))
               (line-offset point (- line-number cur-linum))
               (line-offset (buffer-end point) (- line-number nlines)))))))

(defun set-current-mark (point)
  "Set 'point' to the current mark."
  (let ((buffer (point-buffer point)))
    (mark-set-point (buffer-mark-object buffer) point))
  point)

(defun blank-line-p (point)
  "If the line containing 'point' is only blank, it returns the number of blanks, otherwise it returns nil."
  (let ((string (line-string point))
        (eof-p (last-line-p point))
        (count 0))
    (loop :for c :across string
          :do (unless (or (char= c #\space)
                          (char= c #\tab))
                (return-from blank-line-p nil))
              (incf count))
    (if eof-p
        count
        (1+ count))))

(defun skip-chars-internal (point test dir)
  (loop :for count :from 0
        :for c := (character-at point (if dir 0 -1))
        :do (when (or (null c)
                      (not (if (listp test)
                               (member c test)
                               (funcall test c))))
              (return count))
            (unless (character-offset point (if dir 1 -1))
              (return count))))

(defun skip-chars-forward (point test)
  "From 'point', the character at that position is evaluated by 'test' and moved in the following direction during non-NIL.
If 'test' is a list of characters, is the character at that position in the list of 'test'?
If 'test' is a function, it takes one character at that position as an argument and returns non-NIL."
  ;; `point`からその位置の文字を`test`で評価して非NILの間、後の方向に移動します。
  ;; `test`が文字のリストならその位置の文字が`test`のリスト内に含まれるか
  ;; `test`が関数ならその位置の文字を引数として一つ取り、返り値が非NILであるか
  (skip-chars-internal point test t))

(defun skip-chars-backward (point test)
  "Move the character before the position from 'point' to the previous direction while non-NIL by evaluating it with \"test\".
If 'test' is a list of characters, is the character before that position in the list of 'test'?
If 'test' is a function, it takes one of the characters before its position as its argument and returns non-NIL."
  ;; `point`からその位置の前の文字を`test`で評価して非NILの間、前の方向に移動します。
  ;; `test`が文字のリストならその位置の前の文字が`test`のリスト内に含まれるか
  ;; `test`が関数ならその位置の前の文字を引数として一つ取り、返り値が非NILであるか
  (skip-chars-internal point test nil))

(defun insert-buffer (point buffer)
  "Insert the text in 'buffer' at the position of 'point'.
The difference from 'insert-string' is that the text properties in 'buffer' are also reflected."
  (loop :for line := (point-line (buffer-start-point buffer)) :then (line:line-next line)
        :while line
        :do (insert-string point (line:line-string line))
            (setf (line:line-plist (point-line point)) (line:line-plist line))
            (insert-character point #\newline)))

(defun buffer-text (buffer)
  (points-to-string (buffer-start-point buffer)
                    (buffer-end-point buffer)))
