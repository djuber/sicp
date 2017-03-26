(in-package :sicp)

;; *Exercise 1.11:* A function f is defined by the rule that f(n) = n
;; if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3.
;; Write a procedure that computes f by means of a recursive process.
;; Write a procedure that computes f by means of an iterative
;; process.

;; tree recursive version
(defun f (n)
  (if (< n 3)
      n
      (+
       (* 1 (f (- n 1)))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

;; iterative version, bring the last three answers forward at each step
(defun f-iter (n)
  (labels ((iter (count last1 last2 last3)
	     (if (= count n)
		 (+ last1 (* last2 2) (* last3 3))
		 (iter (1+ count)
		       (+ last1 (* last2 2) (* last3 3))
		       last1
		       last2))))
    (if (< n 3)
	n
	(iter 3 2 1 0))))

