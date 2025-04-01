
#|
aaaaaa
bbbbbb
cccccc
|#

#|********()
|#


(aaaaaaa(
         a
         b
         c
         ))

(defun foo (foo &optional opt1
                          opt2
                &rest rest)
  (list foo opt1 opt2
        rest))

(defmacro foo ((foo &optional opt1
                              opt2
                    &rest rest))
  (list foo opt1 opt2
        rest))

(#+sbcl foo
 #+ccl bar)

(let ((x y)
      (foo #-foo (no-foo)
           #+foo (yes-foo))
      (bar #-bar
           (no-bar)
           #+bar
           (yes-bar)))
  (list foo bar
        x))

(list ;comment
 foo
 bar)

(defun foo (x)
  (tagbody
   foo
    (bar)
   baz
    (when (losing)
      (with-big-loser
          (yow)
        ((lambda ()
           foo)
         big)))
    (flet ((foo (bar baz zap)
             (zip))
           (zot ()
             quux))
      (do ()
          ((lose)
           (foo 1))
        (quux)
        foo
        (lose))
      (cond ((x)
             (win 1 2
                  (foo)))
            (t
             (lose
              3))))))

(defmacro foo (body)
  `(let (,@(stuff)
         ,(more-stuff)
         ,(even-more)
         (foo foo))
     ,@bofy))

(defun foo ()
  `(list foo bar
         ,@(quux fo
                 foo)))

(defmacro foofoo (body)
  `(foo
    `(let (,',@,(stuff)
           ,(more-stuff)
           ,(even-more)
           (foo foo))
       ,@bofy)))

(loop for i from 0 below 2
      for j from 0 below 2
      when foo
      do (fubar)
         (bar)
         (moo)
      and collect cash
      into honduras
      else do ;; this is the body of the first else
              ;; the body is ...
         (indented to the above comment)
         (ZMACS gets this wrong)
      and do this
      and do that
      and when foo
      do the-other
      and cry
      when this-is-a-short-condition do
         (body code of the when)
      when here's something I used to botch do (here is a body)
                                               (rest of body indented same)
      do
         (exdented loop body)
         (I'm not sure I like this but it's compatible)
      when funny-predicate do ;; Here's a comment
         (body filled to comment))

(loop for x in foo1
      for y in quux1
      )

(loop for x in foo
      for y in quux
      finally (foo)
              (fo)
              (zoo)
      do
         (print x)
         (print y)
         (print 'ok!))

(loop for f in files
      collect (open f
                    :direction :output)
      do (foo) (bar)
         (quux))

(loop (foo)
      ;; comment
      (bar)
      (quux))

(loop ;; comment
      (foo)
      (bar))

(loop
  (foo)
  ;; comment
  (bar))

(loop
  ;; comment
  (foo)
  (bar))

(loop ;; comment at toplevel of the loop
      with foo = t
      do (foo foo)
         (foo))

(loop
  ;; comment at toplevel of the loop
  with foo = t
  do (foo foo))

(loop
  ;; comment at toplevel of the loop
  with foo = t
  do (foo foo)
     (foo))

(loop with foo = t
      do (foo foo)
         ;; comment inside clause
         (bar))

(progn
  (loop
    repeat 1000
    do ;; This is the
       ;; beginning
       (foo))
  (loop repeat 100 ;; This too
                   ;; is a beginning
        do (foo)))

(loop
  for foo in bar
  do
     (list foo
           bar
           baz))


(defmethod foo
    ()
  ()
  ())

(defmethod foo :after
    ()
  ()
  ())

(defgeneric foo ()
  (:method
      ()
    1
    2))

(defgeneric foo ()
  (:method :after
      ()
    1
    2))

(defun foo (a
            &optional b
                      c))

(defun foo (a
            &optional
            b
            c))

(defun foo (a &optional
              b
              c))
