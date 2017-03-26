(defpackage "SICP"
  (:use :cl)
  (:shadow
   cl:pi
   cl:abs
   cl:sqrt
   )
  (:export
   :abs
   :average
   :area
   :circumference
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
