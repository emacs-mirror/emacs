(in-package :lem/buffer/internal)

(defclass tmlanguage (syntax-parser)
  ((patterns
    :initarg :patterns
    :accessor tmlanguage-patterns)
   (repository
    :initarg :repository
    :accessor tmlanguage-repository)))

(defclass tm-rule ()
  ((name
    :initarg :name
    :initform 0
    :reader tm-rule-name)))

(defclass tm-region (tm-rule)
  ((begin
    :initarg :begin
    :reader tm-region-begin)
   (end
    :initarg :end
    :reader tm-region-end)
   (begin-captures
    :initarg :begin-captures
    :initform nil
    :reader tm-region-begin-captures)
   (end-captures
    :initarg :end-captures
    :initform nil
    :reader tm-region-end-captures)
   (content-name
    :initarg :content-name
    :initform nil
    :reader tm-region-content-name)
   (patterns
    :initarg :patterns
    :initform nil
    :reader tm-region-patterns)))

(defclass tm-match (tm-rule)
  ((matcher
    :initarg :matcher
    :initform nil
    :reader tm-match-matcher)
   (captures
    :initarg :captures
    :initform nil
    :reader tm-match-captures)
   (move-action
    :initarg :move-action
    :initform nil
    :reader tm-match-move-action)))

(defclass tm-include () ())
(defclass tm-include-self (tm-include) ())
(defclass tm-include-repository (tm-include)
  ((refer
    :initarg :refer
    :reader tm-include-refer)))

(defclass tm-patterns ()
  ((patterns
    :initarg :patterns
    :accessor patterns)))

(defun make-tmlanguage (&key (patterns (make-tm-patterns)) (repository (make-tm-repository)))
  (make-instance 'tmlanguage
                 :patterns patterns
                 :repository repository))

(defun make-tm-repository ()
  (make-hash-table :test 'equal))

(defun make-tm-match (string &key name captures move-action)
  (make-instance 'tm-match
                 :matcher (ppcre:create-scanner string)
                 :name name
                 :captures captures
                 :move-action move-action))

(defun make-tm-region (begin end
                       &key begin-captures end-captures captures
                            name content-name (patterns (make-tm-patterns)))
  (make-instance 'tm-region
                 :begin (ppcre:create-scanner begin)
                 :end (let ((tree (if (stringp end)
                                      (ppcre:parse-string end)
                                      end)))
                        (if (find-tree :back-reference tree)
                            tree
                            (ppcre:create-scanner end)))
                 :begin-captures (or begin-captures captures)
                 :end-captures (or end-captures captures)
                 :name name
                 :content-name content-name
                 :patterns patterns))

(defun make-tm-include (spec)
  (setf spec (coerce spec 'simple-string))
  (cond ((string= spec "$self")
         (make-instance 'tm-include-self))
        ((char= #\# (schar spec 0))
         (make-instance 'tm-include-repository :refer (subseq spec 1)))
        (t
         (error "unsupported spec: ~A" spec))))

(defun make-tm-patterns (&rest patterns)
  (make-instance 'tm-patterns :patterns patterns))

(defun make-tm-name (name)
  name)

(defun add-tm-repository (repository name patterns)
  (setf (gethash name repository) patterns))

(defun add-tm-pattern (tmlanguage pattern)
  (push pattern (patterns (tmlanguage-patterns tmlanguage))))

(defmethod %syntax-scan-region ((tmlanguage tmlanguage) start end)
  (tm-syntax-scan-region start end))


(defun set-syntax-context (line x)
  (setf (line:line-syntax-context line) x))

(defun get-syntax-context (line)
  (line:line-syntax-context line))

(defun tm-get-repository (name)
  (gethash name (tmlanguage-repository (current-syntax-parser))))

(defun tm-ahead-matcher (rule)
  (etypecase rule
    (tm-region
     (tm-region-begin rule))
    (tm-match
     (tm-match-matcher rule))))

(defun tm-ahead-match (rule matcher string start end)
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan matcher string :start start :end (or end (length string)))
    (when start
      (vector rule start end reg-starts reg-ends nil))))

(defun tm-result-rule       (result) (aref result 0))
(defun tm-result-start      (result) (aref result 1))
(defun tm-result-end        (result) (aref result 2))
(defun tm-result-reg-starts (result) (aref result 3))
(defun tm-result-reg-ends   (result) (aref result 4))
(defun tm-result-option     (result) (aref result 5))
(defun (setf tm-result-option) (v result) (setf (aref result 5) v))

(defun tm-result= (result1 result2)
  (and (eq (tm-result-rule result1)
           (tm-result-rule result2))
       (= (tm-result-start result1)
          (tm-result-start result2))
       (= (tm-result-end result1)
          (tm-result-end result2))
       (equal (tm-result-reg-starts result1)
              (tm-result-reg-starts result2))
       (equal (tm-result-reg-ends result1)
              (tm-result-reg-ends result2))))

(defun tm-get-best-result (old-result new-result)
  (if (and new-result
           (or (null old-result)
               (> (tm-result-start old-result)
                  (tm-result-start new-result))
               (and (= (tm-result-start old-result)
                       (tm-result-start new-result))
                    (< (tm-result-end old-result)
                       (tm-result-end new-result)))))
      new-result
      old-result))

(defun tm-best-rule-in-patterns (patterns string start end)
  (macrolet ((add (x)
               (alexandria:once-only (x)
                 `(when ,x (push ,x results) ,x)))
             (add* (x)
               `(multiple-value-bind (result results2) ,x
                  (setf results (nconc results2 results))
                  result)))
    (let ((results '())
          (best))
      (dolist (pattern (patterns patterns))
        (let ((result
                (etypecase pattern
                  (tm-region
                   (add (tm-ahead-match pattern (tm-region-begin pattern) string start end)))
                  (tm-match
                   (add (tm-ahead-match pattern (tm-match-matcher pattern) string start end)))
                  (tm-include-repository
                   (add* (tm-best-rule-in-patterns (tm-get-repository (tm-include-refer pattern))
                                                   string start end)))
                  (tm-include-self
                   (add* (tm-best-rule-in-patterns (tmlanguage-patterns (current-syntax-parser))
                                                   string start end))))))
          (when result
            (setf best (tm-get-best-result best result)))))
      (values best results))))

(defun tm-recompute-results (results string start end)
  (let ((best))
    (loop :for rest-results :on results
          :for result := (car rest-results)
          :do (if (and result (>= start (tm-result-start result)))
                  (let ((new-result
                          (tm-ahead-match (tm-result-rule result)
                                          (tm-ahead-matcher (tm-result-rule result))
                                          string
                                          start
                                          end)))
                    (setf (car rest-results) new-result)
                    (setf best (tm-get-best-result best new-result)))
                  (setf best (tm-get-best-result best result))))
    best))

(defun tm-apply-capture (point capture start end)
  (typecase capture
    (tm-patterns
     (tm-scan-line point capture start end))
    (otherwise
     (line:line-add-property (point-line point) start end :attribute capture nil))))

(defun tm-apply-captures (point result captures)
  (when (and captures (< 0 (length captures)))
    (let ((start (tm-result-start result))
          (end (tm-result-end result))
          (reg-starts (tm-result-reg-starts result))
          (reg-ends (tm-result-reg-ends result)))
      (alexandria:when-let ((cap0 (aref captures 0)))
        (tm-apply-capture point cap0 start end))
      (loop :for reg-start :across reg-starts
            :for reg-end :across reg-ends
            :for capture-index :from 1 :below (length captures)
            :for cap := (aref captures capture-index)
            :do (when (and cap reg-start)
                  (tm-apply-capture point cap reg-start reg-end))))))

(defun tm-apply-content-name (rule point start end contp)
  (alexandria:when-let (content-name (tm-region-content-name rule))
    (line:line-add-property (point-line point) start end
                       :attribute content-name
                       contp)))

(defun tm-replace-back-reference (tree string reg-starts reg-ends)
  (loop :with copied := nil
        :for n :from 1
        :for reg-start :across reg-starts
        :for reg-end :across reg-ends
        :do (let ((str (subseq string reg-start reg-end)))
              (unless copied
                (setf copied t)
                (setf tree (copy-tree tree)))
              (setf tree
                    (nsubst-if str
                               (lambda (x)
                                 (and (consp x)
                                      (eq :back-reference (first x))
                                      (= n (second x))))
                               tree))))
  tree)

(defun tm-init-region-end-regex (rule begin-result string)
  (let* ((reg-starts
           (tm-result-reg-starts begin-result))
         (reg-ends
           (tm-result-reg-ends begin-result))
         (end-regex
           (tm-replace-back-reference (tm-region-end rule)
                                      string
                                      reg-starts
                                      reg-ends)))
    (setf (tm-result-option begin-result)
          (ppcre:create-scanner end-regex))))

(defun tm-apply-begin-captures (rule point begin-result start-line-p)
  (when start-line-p
    (let ((captures (tm-region-begin-captures rule)))
      (tm-apply-captures point begin-result captures))))

(defun tm-apply-end-captures (rule point end-result)
  (let ((captures (tm-region-end-captures rule)))
    (tm-apply-captures point end-result captures)))

(defun tm-apply-region (rule point begin-result end start-line-p)
  (when start-line-p
    (tm-init-region-end-regex rule begin-result (line-string point)))
  (let ((start1 (if start-line-p (tm-result-start begin-result) 0))
        (start2 (if start-line-p (tm-result-end begin-result) 0)))
    (let ((end-result
            (tm-ahead-match rule (tm-result-option begin-result) (line-string point) start2 end)))
      (multiple-value-bind (best results)
          (tm-best-rule-in-patterns (tm-region-patterns rule)
                                    (line-string point)
                                    start2 nil)
        (setf best (tm-get-best-result best end-result))
        (loop
          (cond ((null best)
                 (line:line-add-property (point-line point) start1 (line:line-length (point-line point))
                                    :attribute (tm-rule-name rule)
                                    t)
                 (tm-apply-begin-captures rule point begin-result start-line-p)
                 (tm-apply-content-name rule point start2 (line:line-length (point-line point)) t)
                 (set-syntax-context (point-line point) (cons rule begin-result))
                 (line-end point)
                 (return))
                ((and best end-result (tm-result= best end-result))
                 (line:line-add-property (point-line point) start1 (tm-result-end end-result)
                                    :attribute (tm-rule-name rule)
                                    nil)
                 (tm-apply-begin-captures rule point begin-result start-line-p)
                 (tm-apply-end-captures rule point end-result)
                 (tm-apply-content-name rule point start1 (tm-result-start end-result) nil)
                 (set-syntax-context (point-line point) nil)
                 (line-offset point 0 (tm-result-end end-result))
                 (return))
                (t
                 (line-offset point 0 (tm-result-end best))
                 (setf end-result
                       (tm-ahead-match rule
                                       (tm-result-option begin-result)
                                       (line-string point)
                                       (point-charpos point)
                                       end))
                 (setf best
                       (tm-get-best-result
                        end-result
                        (tm-recompute-results results
                                              (line-string point)
                                              (point-charpos point)
                                              end))))))))))

(defun tm-move-action (rule point allow-multiline)
  (with-point ((start point)
               (end point))
    (let ((end (funcall (tm-match-move-action rule) end)))
      (when (and end (point< point end))
        (loop :until (same-line-p point end)
              :do (set-syntax-context (point-line point) rule)
                  (line-offset point 1))
        (set-syntax-context (point-line point) 'end-move-action)
        (alexandria:when-let ((attribute (tm-rule-name rule)))
          (put-text-property start end :attribute attribute))
        (cond (allow-multiline
               (move-point point end))
              ((same-line-p start end)
               (move-point point end))
              (t
               (line-end (move-point point start))))))))

(defun tm-apply-match-in-capture (point capture start end)
  (typecase capture
    (tm-patterns
     (tm-scan-line point capture start end))
    (otherwise
     (line:line-add-property (point-line point) start end :attribute capture nil))))

(defun tm-apply-match (rule point result)
  (let ((start (tm-result-start result))
        (end (tm-result-end result))
        (captures (tm-match-captures rule)))
    (line:line-add-property (point-line point) start end :attribute (tm-rule-name rule) nil)
    (tm-apply-captures point result captures)
    (cond ((tm-match-move-action rule)
           (line-offset point 0 start)
           (or (tm-move-action rule point nil)
               (line-offset point 0 end)))
          (t
           (line-offset point 0 end)))))

(defun tm-apply-result (point result end)
  (let ((rule (tm-result-rule result)))
    (etypecase rule
      (tm-region
       (tm-apply-region rule point result end t))
      (tm-match
       (tm-apply-match rule point result)))))

(defun tm-continue-prev-line (point)
  (let* ((line (point-line point))
         (prev (line:line-previous line))
         (context (and prev (get-syntax-context prev)))
         (rule (alexandria:ensure-car context)))
    (cond ((null rule)
           (set-syntax-context line nil))
          ((typep rule 'tm-region)
           (tm-apply-region rule point
                            (when (consp context) (cdr context))
                            nil nil))
          ((typep rule 'tm-rule)
           (cond ((eq (get-syntax-context line) 'end-move-action)
                  (with-point ((p point))
                    (previous-single-property-change p :attribute)
                    (let ((goal (tm-move-action rule p t)))
                      (when goal
                        (move-point point goal)))))
                 (t
                  (line:line-add-property (point-line point)
                                     0 (line:line-length line)
                                     :attribute (tm-rule-name rule)
                                     t)
                  (line-end point))))
          (t
           (set-syntax-context line nil)))))

(defun tm-scan-line (point patterns start end)
  (let ((old-linenumber (line-number-at-point point)))
    (multiple-value-bind (best results)
        (tm-best-rule-in-patterns patterns (line-string point) start end)
      (loop
        (unless best (return))
        (tm-apply-result point best end)
        (setf best (tm-recompute-results results (line-string point) (point-charpos point) end))))
    (assert (= old-linenumber (line-number-at-point point)))
    (when (and end (< end (point-charpos point)))
      (line-offset point 0 end))))

(defun tm-syntax-scan-line (point)
  (tm-continue-prev-line point)
  (tm-scan-line point
                (tmlanguage-patterns (current-syntax-parser))
                (point-charpos point)
                nil)
  (line-offset point 1))

(defun tm-syntax-scan-region (start end)
  (loop
    (line:line-clear-property (point-line start) :attribute)
    (unless (tm-syntax-scan-line start)
      (return start))
    (when (point<= end start)
      (return start))))
