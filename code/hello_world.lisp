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
