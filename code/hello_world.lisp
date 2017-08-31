(defun hello-world ()
  (format t "hello , world"))

( * 2 2)

(defun square (x)
  (* x x))


 (defun triple (x)
	  (* x x x))
(list :a 1 :b 2 :c 3)
(getf (list :a 1 :b 2 :c 3) :a)
(getf (list :a 1 :b 2 :c 3) :b)
(defun make-cd (title artist rating ripped)
	   (list :title title :artist artist :rating rating :ripped ripped))
(make-cd "culture" "migos" 10 t)
(defvar *db* nil)
(defun add-record (cd) (push cd *db*))
(add-record (make-cd "culture" "migos" 10 t))
(add-record (make-cd "dank" "meh" 12 t))
(add-record (make-cd "hello" "world" 9 t))
(defun dumb-db ()
	   (dolist (cd *db*)
	     (format t "~{~a:~10t~a~%~}~%" cd)))
(atom *db*)
(symbolp *db*)
(atom 'a)
(atom 'b)
(atom 'c)
(atom 'zuriel)
(atom '(zuriel))
(atom '(zuriel victory laptop chair))
(consp '(zuriel victory laptop chair))
(consp *db*)
(defvar x 2)
(defvar x 3)
(defparameter x 2)
(defparameter x 3)
(consp x)
(defparameter objects-around-me '(phone mirror mattress pen))
(atom objects-around-me)
(defvar objects-around-me '(phone mirror mattress pen))
(consp objects-around-me)
*db*
(atom x)
'(+ 1 2)
(defparameter x (+ 3 5))
(defun dumb-db ()
  (format t "~{~{~a:10t~%~}~%~}" *db*))
(defun prompt-read (prompt)
	   (format *query-io* "~a: " prompt)
	   (force-output *query-io*)
	   (read-line *query-io*))
(defun prompt-for-cd ()
	   (make-cd
	    (prompt-read "Title")
	    (prompt-read "Artist")
	    (prompt-read "Rating")
	    (prompt-read "Ripped [y/n]")))
;(parse-integer (prompt-read "Rating") :junk-allowed t)
;(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
(defun prompt-for-cd ()
	   (make-cd
	    (prompt-read "Title")
	    (prompt-read "Artist")
	    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
	    (y-or-n-p "Ripped [y/n]")))
(defun add-cds ()
	   (loop (add-record (prompt-for-cd))
	      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))
(defun save-db (filename)
	   (with-open-file (out filename
				:direction :output
				:if-exists :supersede)
	     (with-standard-io-syntax
	       (print *db* out))))
(save-db "~/LearningEthereum/code/my-cds.db")
(defun load-db (filename)
	   (with-open-file (in filename)
	     (with-standard-io-syntax
	       (setf *db* (read in)))))
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(mapcar #'square '(1 2 3 4 5 6))
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(defun our-evenp (x)
  (= 0 (mod x 2)))
(remove-if-not
 #'(lambda (cd) (equal (getf cd :artist)  "migos")) *db*)
(defun select-by-artist (artist)
	   (remove-if-not
	    #'(lambda (cd) (equal (getf cd :arist) artist))
	    *db*))
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
;(select #'(lambda (cd) (equal (getf cd :artist) "migos")))
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
(defun foo (a b c) (list a b c))
(defun foo (&key a b c) (list a b c))
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
	   (setf *db*
		 (mapcar
		  #'(lambda (row)
		      (when (funcall selector-fn row)
			(if title (setf (getf row :title) title))
			(if artist (setf (getf row :artist) artist))
			    (if rating (setf (getf row :rating) rating))
			    (if ripped-p (setf (getf row :ripped) ripped)))row) *db*)))
(defun where (&key title artist rating (ripped nil ripped-p))#'(lambda (cd)
									  (and 
									   (if title (equal (getf cd :title) title) t)
									   (if artist (equal (getf cd :artist) artist) t)
									   (if rating (equal (getf cd :rating) rating) t)
									   (if ripped-p (equal (getf  cd :ripped) ripped) t))))
(defun where (&key title artist rating (ripped nil ripped-p))#'(lambda (cd)
									  (and 
									   (if title (equal (getf cd :title) title) t)
									   (if artist (equal (getf cd :artist) artist) t)
									   (if rating (equal (getf cd :rating) rating) t)
									   (if ripped-p (equal (getf  cd :ripped) ripped) t))))(defun where (&key title artist rating (ripped nil ripped-p))#'(lambda (cd)
									  (and 
									   (if title (equal (getf cd :title) title) t)
									   (if artist (equal (getf cd :artist) artist) t)
									   (if rating (equal (getf cd :rating) rating) t)
									   (if ripped-p (equal (getf  cd :ripped) ripped) t))))
(defun where (&key title artist rating (ripped nil ripped-p))#'(lambda (cd)
									  (and 
									   (if title (equal (getf cd :title) title) t)
									   (if artist (equal (getf cd :artist) artist) t)
									   (if rating (equal (getf cd :rating) rating) t)
									   (if ripped-p (equal (getf  cd :ripped) ripped) t))))
(defun delete-rows (selector-fn)
	   (setf *db* (remove-if selector-fn *db*)))
(reverse '(1 2 3))
(defmacro backwards (expr) (reverse expr))
(defmacro backwards (expr) (reverse expr))(backwards ("hello, world" t format))
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
;(make-comparison-expr :rating 10)
'(1 2 (+ 1 2))
(defun make-comparisons-list (fields)
	   (loop while fields
		collecting (make-comparison-expr (pop fields) (pop fields))))
(defmacro where (&rest clauses)
  ` #'(lambda (cd) (and ,@(make-comparisons-list clauses))))

`(and ,(list 1 2 3))
(symbolp #'square)
(where :title "migos" :ripped t)
(macroexpand-1 '(where :title "culture" :title t))
;(select (where :title "culture" :ripped t))

;;REPL: Read Evaluate Print Loop

(defun zuriels-programming-language ()
  (format t "~&zuriels-repl> ")
  (print (eval (read)))
  (zuriels-programming-language))
(defun name (parameter*)
	   "Optional documentation string."
	   body-form*)
(defun verbose-sum (x y)
	   "sum any two numbers after printing a message."
	   (format t "summing `d and `d. `%" x y)
	   (+ x y))
(defun foo (a b &optional c d) (list a b c d))
(defun foo (a &optional (b 10)) (list a b))
(defun make-rectangle  (width &optional (height width)))
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))
(defun foo (&key a b c) (list a b c))
(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
	   (list a b c b-supplied-p))
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))
(defun foo (x &optional y &key z) (list x y z))
(defun foo (&rest rest &key a b c) (list rest a b c))
(defun  foo (n)
	   (dotimes (i 10)
	     (when (> (* i  j) n)
	       (return-from foo (list i j)))))
(defun foo (x) (* 2 x))
(function foo)
#'foo
(defun plot (fn min  max step)
	   (loop for i from min to max by step do
		(loop repeat (funcall fn i) do (format t "*"))
		(format  t "~%")))
(plot #'exp 0 4 1/2)
(defun f (x y)
  (+ (square x) (square y)))
(f 2 3)
((lambda (x y) (+ x y)) 2 3)
(defun double1 (x)
	   (* 2))
(plot #'double1 0 10 1)
(defun foo (x y z) (+ x y z))
(defun foo (x y z) (+ x y z))(defun foo (x)
	   (format t "Parameter: ~a~%" x)
	   (let ((x 2))
	     (format t "Outer LET: ~a~%" x)
	     (let ((x 3))
	       (format t "Inner LET: ~a~%" x))
	     (format t "Outer LET: ~a~%" x))
	   (format t "Parameter: ~a~%" x))
(dotimes (x 10) (format t "~d " x))
(let*  ((x 10)
		(y (+ x 10)))
  (list x y))
(let ((count 0)) #'(lambda () (setf count (1+ count))))
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
(let ((count 0))
	   (list
	    #'(lambda () (incf count))
	       #'(lambda () (decf count))
	       #'(lambda () count)))
(defvar *count* 0
  "Count of widgets made.")
(defparameter *gap-tolerance*.001
  "Tolerance to be allowed.")
(defun incriment-widget-count () (incf *count*))
(defvar *x* 10)
(defun foo () (format t "X: ~d~%" *x*))
(let ((*x* 20)) (foo))
(defun bar ()
	   (foo)
	   (let ((*x* 20)) (foo))
	   (foo))
(defun foo ()
	   (format t "Before assignment~18tX: ~d~%" *x*)
	   (setf *x* (+ 1 *x*))
	   (format t "After assignment`18tX: ~d~%" *x*))
(setf x 10)
(setf x 10)
(let ((y 20))
	   (foo y)
	   (print y))
(setf x 1 y 2)
(setf x (setf y (random 10)))
(setf x (+ x 1))
(incf x)
(if (> 2 3) "Yup" "Nope")
(if (> 3 2) "Yup" "Nope")
`(defmacro unless (condition &rest body)
	   `(if (not ,condition) (progn ,@body)))
(dolist (x '(1 2 3)) (print x))
(dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))
(dotimes (i 4) (print i))
(dotimes (x 20)
	  (dotimes (y 20)
	    (format t "~3d " (* (1+ x) (1+ y))))
	  (format t "~%"))
(do ((n 0 (1+ n))
	      (cur 0 next)
	      (next 1 (+ cur next)))
    ((= 10 n) cur))
(do ((i 0 (1+ i)))
	     ((>= i 4))
  (print i))
(dotimes (i 4) (print i))
(do ((n 0 (1+ n))
	      (cur 0 next)
	      (next 1 (+ cur next)))
    ((= 10 n) cur))
(do ((nums nil) (i 1 (1+ i)))
	     ((> i 10) (nreverse nums))
  (push i nums))
(loop for i from 1 to 10 collecting i)
(loop for x from 1 to 10 summing (expt x 2))
(loop for x across "the quick brown fox jumps over the lazy dog"
   counting (find x "aeiou"))
(loop for i below 10
	      and a = 0 then b
	      and b = 1 then (+ b a)
	      finally (return a))
(defun foo (x)
  (when (> x 10) (print 'big)))
(if (> x 10) (progn (print 'big)))
(defun primep (number)
	   (when (> number 1)
	     (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))
(defun next-prime (number)
  (loop for n from number when (primep n) return n))
(do ((p (next-prime 0) (next-prime (1+ p))))
	     ((> p 19))
  (format t " ~d " p))
(defmacro do-primes (var-and-range &rest body)
	   (let ((var (first var-and-range))
		 (start (second var-and-range))
		 (end (third var-and-range)))
	     `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))0
		   ((> ,var ,end))
		   ,@body))))
(defmacro do-primes ((var start end) &body body)
	   `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
		((> ,var ,end))
	      ,@body))
(defmacro do-primes-a ((var start end) &body body)
	   (append '(do)
		   (list (list (list var
				     (list 'next-prime start)
				     (list 'nxt-prime (list '1+ var)))))
		   (list (list (list '> var end)))
		   body))
(macroexpand-1 '(do-primes (p 0 19) (format t "~d" p)))
(do-primes (p 0 (random 100))
  (format t "~d" p))
(macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d" p)))
(defmacro do-primes ((var start end) &body body)
`(do ((ending-value ,end)
      (,var (next-prime ,start) (next-prime (1+ ,var))))
     ((> ,var ending-value))
   ,@body))
(defmacro do-primes ((var start end) &body body)
	   `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
		 (ending-value ,end))
		((> ,var ending-value))
	      ,@body))
(defmacro do-primes ((var start end) &body body)
	   (with-genysms (ending-value-name)
	     `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
		   (,var ,ending-value-name))
		  ,@body)))
(defmacro with-gensyms ((&rest names) &body body)
	   `(let ,(loop for n in names collect `(,n (gensym)))
	      ,@body))
(loop for n in '(a b c) collect `(,n (gensym)))
(defun test-+ ()
	   (and
	    (= (+ 1 2) 3)
	    (= (+ 1 2 3) 6)
	    (= (+ -1 -3) -4)))
(defun test-+ ()
	   (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) `(= (+ 1 2) 3))
	   (format t "~:[FAIL~;pass~] ...~a~%" (= (+ 1 2 3) 6) `(= (+ 1 2 3) 6))
	   (format t "~:[FAIL~;passs~] ...~a~%" (= (+ -1 -3) -4) `(= (+ -1 -3) -4)))
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ...~a~%" result form))
(defun test-+ ()
	   (report-result (= (+ 1 2) 3) `(= ( 1 2) 3))
	   (report-result (= (+ 1 2 3) 6) `(= (+ 1 2 3) 6))
	   (report-result (= (+ -1 -3) -4) `(= (+ -1 -3) -4)))
(defmacro check (form)
  `(report-result ,form `,form))
(defun test-+ ()
	   (check (= (+ 1 2) 3))
	   (check (= (+ 1 2 3) 6))
	   (check (= (+ -1 -3) -4)))
(defmacro check (&body forms)
	   `(progn
	      ,@(loop for f in forms collect `(report-result ,f))))
(defun test-+ ()
	   (check
	     (= (+ 1 2) 3)
	     (= (+ 1 2 3) 6)
	     (= (+ -1 -3) -4)))
(defun report-result (result form)
	   (format t"~:[FAIL~;pass~] ... ~a~%" result form)
	   result)
(defmacro combine-results (&body forms)
	   (with-gensyms (result)
	     `(let ((,result t))
		,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
		,result)))
(defmacro check (&body forms)
	   `(combine-results
	      ,@(loop for f in forms collect `(report-result ,f `,f))))
(defun test-* ()
	   (check
	     (= (* 2 2) 4)
	     (= (* 3 5) 15)))
(defun test-arithmetic ()
	   (combine-results
	     (test-+)
	     (test-*)))
(defvar *test-name* nil)
;;(format t "~[FAIL~;pass~] ...~a~%" result *test-name* form)
(defun test-+ ()
	   (let ((*test-name* 'test-+))
	     (check
	     (= (+ 1 2) 3)
	     (= (+ 1 2 3) 6)
	     (= (+ -1 -3) -4))))
(defun test-* ()
	   (let ((*test-name* 'test-*))
	     (check
	       (= (* 2 2) 4)
	       (= (* 3 5) 15))))
(defmacro deftest (name parameters &body body)
	   `(defun ,name ,parameters
	     (let ((*test-name* ',name))
	       ,@body)))
(deftest test-+ ()
	   (check
	     (= (+ 1 2) 3)
	     (= (+ 1 2 3) 6)
	     (= (+ -1 -3) -4)))
;;(let ((*test-name* (append *test-name* (list `,name))))
(deftest test-arithmetic ()
	   (combine-results
	     (test-+)
	     (test-*)))
#xa
#b101010
#(2   1)
(2/3   3/4)
(+ 10.0 3.0)
(+ #c(1 2) #c(3 4))
(- 5 4)
(- 2)
(+ 1 2.0)
(/ 2 3.0)
(+ #c(1 2) 3)
(+ #c(1 2) 3/2)
(+ #c(1 1) #c(2 -1))
(= 1 1)
(= 10 20/2)
(= 1 1.0 #c(1.0 0.0) #c(1 0))
(/= 1 1)
(< 2 3)
(> 2 3)
(max 100 200)
(min 200 1238)
"foo/bar"
(string= "foodbarbaz" "quuxbarfoo" :start1 3 :end1 6 :start2 4 :end2 7)
(string/= "lisp" "lissome")
(string< "lisp" "lisper")
(string< "foobar" "abaz" :start1 3 :start2 1)
(vector)
(vector 1)
(vector 1 2)
(make-array 5 :initial-element nil)
(make-array 5 :fill-pointer 0)
(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*)
(vector-push 'b *x*)
(vector-push 'c *x*)
(vector-pop *x*)
(vector-pop *x*)
(vector-pop *x*)
(make-array 5 :fill-pointer 0 :adjustable t)
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)
(defparameter *x* (vector 1 2 3))
(length *x*)
(elt *x* 0)
(elt *x* 1)
(elt *x* 2)
(setf (elt *x* 0) 10)
(count "foo" #("foo" "bar" "baz") :test #'string=)
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t
 ;;(remove #/a "foobarbaz" :count 1)
 ;;(remove #/a "foobarbaz" :count 1 :from-end t)
      (defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
      (defun verbose-first (x) (format t "Looking at ~s~%" x) (first x))
      (count 'a *v* :key #'verbose-first)
      (count 'a *v* :key #'verbose-first :from-end t)
      (count-if #'evenp #(1 2 3 4 5))
      (count-if-not #'evenp #(1 2 3 4 5))
      (position-if #'digit-char-p "abcd0001")
      ;;(remove-if-not #'(lambda (x) (char= (elt x 0) #/f))
      #("foo" "baz" "foom"))
(concatenate 'vector #(1 2 3) '(4 5 6))
(concatenate 'list #(1 2 3) '(4 5 6))
;;(setf my-sequence (sort my-sequence #'string<))
(merge 'vector #(1 3 5) #(2 4 6) #'<)
(merge 'list #(1 3 5) #(2 4 6) #'<)
(subseq "foobarbaz" 3)
(subseq "foobarbaz" 3 6)
(defparameter *x* (copy-seq "foobarbaz"))
(setf (subseq *x* 3 6) "xxx") ;subsequence new value are same lenght *x*
(setf (subseq *x* 3 6) "abcd") ;subsequence new value too long, extra character ignored
(setf (subseq *x* 3 6) "xx") ;subsequence new value too short, only two characters
;;(position #/b "foobarbaz")
(search "bar" "foobarbaz")
(mismatch "foobarbaz" "foom")
(every #'evenp #(1 2 3 4 5))
(some #'evenp #(1 2 3 4 5))
(notany #'evenp #(1 2 3 4 5))
(notevery #'evenp #(1 2 3 4 5))
(every #'> #(1 2 3 4) #(5 4 3 2))
(some #'> #(1 2 3 4) #(5 4 3 2))
(notany #'> #(1 2 3 4) #(5 4 3 2))
(notevery #'> #(1 2 3 4) #(5 4 3 2))
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6))
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10))
(defun show-value (key hash-table)
	   (multiple-valued-bind (value present) (gethash key hash-table)
				 (if present
				     (format nil "value  ~a actually present.")
				     (format nil "value ~a because key not found." value))))
;;(setf (gethash 'bar *h*) nil) ; provide an explicit value of nil
;;(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
;;(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
(cons 1 2)
(car (cons 1 2))
(cdr (cons 1 2))
(defparameter *cons* (cons 1 2))
(setf (car *cons*) 10)
(setf (cdr *cons*) 20)
(cons 1 nil)
(cons 1 (cons 2 nil))
(cons 1 (cons 2 (cons 3 nil)))
(list 1)
(list 1 2 3 4 5)
(defparameter *list* (list 1 2 3 4))
(first *list*)
(rest *list*)
(first(rest *list*))
(list "foo" (list 1 2) 10)
(append (list 1 2) (list 3 4))
(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (list 1 2 3 4))
(setf (first *list-2*) 0)
(setf *list* (reverse *list*))
(ndonc *x* (list 4 5 6))
(nconc *x* (list 4 5 6))
(defun upto (max)
	   (let ((result nil))
	     (dotimes (i max)
	       (push i result))
	     (nreverse result)))
(upto 12)
(setf *list-3* (delete 4 *list-3*))
(setf *list-3* (delete 4 *list-3*))
(defparameter *list* (list 4 3 2 1))
(sort *list* #'<)
(caar (list  (list 1 2) 3))
(cadr (list (list 1 2) (list 3 4)))
(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3))
(mapcar #'(lambda (x) (* 2 x)) (list 10 20 30))
(mapcar #'+(list 1 2 3) (list 10 20 30))
;;code
(defparameter *set* ())
(adjoin 1 *set*)
(setf *set* (adjoin 1 *set*))
(pushnew 2 *set*)
(subsetp '(3 2 1) '(1 2 3 4))
(subsetp '(1 2 3 4) '(3 2 1))
(assoc 'a `((a , 1) (b , 2) (c , 3)))
(assoc 'c `((a , 1) (b , 2) (c , 3)))
(assoc 'f `((a , 1) (b , 2) (c , 3)))
(cdr (assoc 'a `((a , 1) (b , 2) (c , 3))))
(assoc 'a `((a , 10) (b , 2) (c , 3)))
;;(cons (cons 'new-key 'new-value) alist)
;;(acons 'new-key 'new-vale alist)
(pairlis '(a b c) '(1 2 3))
(defparameter *plist* ())
(setf (getf *plist* :a) 1)
(setf (getf *plist* :a) 2)
(remf *plist* :a)
(defun process-properties (plist keys)
	   (loop while plist do
		(multiple-value-bind (key value tail) (get-properties plist keys)
		  (when key (process-property key value))
		  (setf plist (cddr tail)))))
(setf (get 'some-symbol 'my-key) "information")
;;(destructing-bind (x y z) (list 1 2 3)
(list :x x :y y :z z))
;;(destructing-bind (&whole whole &key x y z) (list :z 1 :y 2 :x 3)
			   (list :x x :y y :z z :whole whole))
