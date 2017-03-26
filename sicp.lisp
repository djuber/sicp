(defpackage "SICP"
  (:use :cl)
  (:shadow
   cl:pi
   cl:abs
   cl:sqrt
   )
  ;; friends alphabetize their exports
  (:export
   :abs
   :average
   :area
   :circumference
   :double
   :pi
   :radius
   :size
   :sqrt
   :square
   :sum-of-squares
   )
  (:documentation "SICP Solutions"))
  
(in-package :sicp)

;; 1.1.2 naming and the environment
(defconstant pi cl:pi)

(defparameter radius 10)

(defparameter size 2)

;; 1.1.4 compound procedures
(defun square (x) (* x x))

(defun circumference (radius)
  (* 2 pi radius))

(defun area (radius)
  (* pi (square radius)))

(defun sum-of-squares (x y)
  (+ (square x)
     (square y)))

(defun square-of-sum (x y)
  (square (+ x y)))

(defun f (a)
  (sum-of-squares (+ a 1) (* a 2)))

;; 1.1.5 substitution model for application

(defun abs (x) ; this, like <=, is predefined in Common Lisp
  (if (< x 0)
      (- x)
      x))

;; 1.1.7 Square roots by newton's method.


;; wishful thinking top level:
#|
(defun sqrt-iter (guess x)
  "if the guess is good enough, stop. Otherwise, repeat with an improved guess"
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))


(defun improve (guess x)
  "a guess is improved by averaging it with the quotient of the 
radicand and the old guess"
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defparameter sqrt-tolerance 0.001)
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) sqrt-tolerance))
|#

;; we can keep this part
(defparameter sqrt-tolerance 0.001)

(defun average (x y)
  (/ (+ x y) 2))

(defun sqrt (x)
  "calculate the square root by successive approximations"
  (flet ((good-enough? (guess)
	   (< (abs (- (square guess) x)) sqrt-tolerance))
	 (improve (guess)
	   (average guess (/ x guess))))
    (labels ((sqrt-iter (guess)
	     (if (good-enough? guess)
		 guess
		 (sqrt-iter (improve guess)))))
      (sqrt-iter 1.0d0)))) ; use a double here.

;; test that this is correct
(< (- (sqrt 2)(cl:sqrt 2)) sqrt-tolerance)
 ; => T

(< (- (sqrt 3)(cl:sqrt 3)) sqrt-tolerance)
 ; => T

(sqrt 9)
 ; => 3.00009155413138d0

(cl:sqrt 9)
 ; => 3.0

;; 1.1.8 procedures as black box abstractions

;; double introduced during a discussion of equivalent formulations for square
(defun double (x) (+ x x))

;; 1.2 Procedures and the processes they generate

;; 1.2.1 Linear Recursion and Iteration

;; simple recursive definition of factorial
(defun fact-1.2.1 (n)
  (if (= n 0)
      1
      (* n (fact-1.2.1 (1- n)))))

;; iterative definition
(defun fact-1.2.1.a (n)
  (labels ((fact-iter (acc count)
	   (if (> count n)
	       acc
	       (fact-iter (* acc count) (+ 1 count)))))
    (fact-iter 1 1)))


;;  In general, an iterative process is one whose state can be
;; summarized by a fixed number of "state variables", together with a
;; fixed rule that describes how the state variables should be updated as
;; the process moves from state to state and an (optional) end test that
;; specifies conditions under which the process should terminate.  In
;; computing n!, the number of steps required grows linearly with n.  Such
;; a process is called a "linear iterative process".

