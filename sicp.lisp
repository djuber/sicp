(defpackage "SICP"
  (:use :cl)
  (:shadow
   cl:pi
   cl:abs
   cl:sqrt
   cl:expt
   cl:gcd
   )
  ;; friends alphabetize their exports
  (:export
   :abs
   :average
   :area
   :circumference
   :double
   :expt
   :gcd
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
; (< (- (sqrt 2)(cl:sqrt 2)) sqrt-tolerance)
 ; => T

; (< (- (sqrt 3)(cl:sqrt 3)) sqrt-tolerance)
 ; => T

; (sqrt 9)
 ; => 3.00009155413138d0

; (cl:sqrt 9)
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

;; 1.2.2 tree recursion

(defun fib* (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(:else (+ (fib (- n 1))
		  (fib (- n 2))))))

;; ((lambda () (trace fib) (fib 5) (untrace fib)))
;; trace does show what looks like the tree diagram in 1.2.2

;; fib(n) is the closest integer to phi^n/sqrt(5)
(defconstant +phi+ (/ (1+ (cl:sqrt 5.0d0)) 2))

(defun fast-fib (n)
  (round (/ (cl:expt +phi+ n) (cl:sqrt 5.0d0))))

(defun fib (n)
  (labels ((iter (a b count)
	     (if (zerop count)
		 b
		 (iter (+ a b) a (- count 1)))))
    (iter 1 0 n)))


;; side-note - machine precision, even with doubles, becomes a
;; problem when n is large enough
 ;; (loop :for i :from 0
 ;;      :until (not (zerop (- (fib i) (fast-fib i))))
 ;;      :finally (return i))
 ; => 71 (7 bits, #x47, #o107, #b1000111)

; (fib 71)
 ; => 308061521170129 (49 bits, #x1182E2989CED1)
; (fast-fib 71)
 ; => 308061521170130, -0.3125d0

;; example : counting change
;; how many ways can we make change for a dollar using half-dollars, quarters,
;; dimes, nickels, and pennies


;; my approach from the problem description
(defun change-for (amount coins)
  (cond
    ((zerop amount) 1)
    ((null coins) 0)  ;; this is the 'kinds-of-coins 0' test
    ((< amount (first coins)) (change-for amount (rest coins)))
    (:else (+
	    (change-for (- amount (first coins)) coins)
	    (change-for amount (rest coins))))))

; (change-for 100 (list 50 25 10 5 1))
					; => 292 (9 bits, #x124)
; (change-for 100 (list 5 25 10 50 1))
 ; => 292 (9 bits, #x124)
; (change-for 100 (list 50 10 1 5 25))
 ; => 292 (9 bits, #x124)
;; order of the coins doesn't matter.

;; the authors approach

(defun count-change (amount)
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(:else (+ (cc amount (- kinds-of-coins 1))
		  (cc (- amount (first-denomination kinds-of-coins))
		      kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

; (count-change 100)
 ; => 292 (9 bits, #x124)

;; 1.2.3 orders of growth

;; see exercises 1.14 and 1.15


;; 1.2.4 Exponentiation

(defun expt* (b n)
  "first approximation of a natural exponent. N must be a whole number."
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(defun expt** (b n)
  "rewrite expt* as an iteration"
  (labels ((iter (counter product)
	     (if (zerop counter)
		 product
		 (iter (1- counter) (* b product)))))
    (iter n 1)))


(defun expt (b n)
  (cond
    ((zerop n) 1)
    ((evenp n) (square (expt b (/ n 2))))
    (:else (* b (expt b (- n 1))))))

;; 1.2.5 Greatest Common Divisor

(defun gcd (a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

;; 1.2.6 Primality tests.

(defun smallest-divisor (n)
  (find-divisor n 2))

;; quick optimization is possible by omitting evens
(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(:else (find-divisor n (1+ test-divisor)))))

(defun divides? (a b)
  (zerop (mod b a)))


(defun prime? (n)
  "this is a slow way to do it."
  (= n (smallest-divisor n)))


;; fermat test for primality

(defun expmod (base expt modulus)
  "calculate (mod (expt bas expt) modulus)"
  (cond ((= expt 0) 1)
	((evenp expt)
	 (mod (square (expmod base (/ expt 2) modulus))
	     modulus))
	(:else
	 (mod (* base (expmod base (- expt 1) modulus))
	     modulus))))


(defun fermat-test (n)
    (flet ((try-it (a)
	     (= (expmod a n n) a)))
      (try-it (+ 1 (random (- n 1))))))

(defun fast-prime? (n times)
  (cond ((= times 0) T)
	((fermat-test n) (fast-prime? n (1- times)))
	(:else nil)))

;; (defun prime? (n)
;;   (fast-prime? n (isqrt n)))
