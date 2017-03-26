(in-package :sicp)

     ;; *Exercise 1.7:* The `good-enough?' test used in computing square
     ;; roots will not be very effective for finding the square roots of
     ;; very small numbers.  Also, in real computers, arithmetic operations
     ;; are almost always performed with limited precision.  This makes
     ;; our test inadequate for very large numbers.  Explain these
     ;; statements, with examples showing how the test fails for small and
     ;; large numbers.  An alternative strategy for implementing
     ;; `good-enough?' is to watch how `guess' changes from one iteration
     ;; to the next and to stop when the change is a very small fraction
     ;; of the guess.  Design a square-root procedure that uses this kind
     ;; of end test.  Does this work better for small and large numbers?


;; multipart question. First is explain and example

;; if we start with 10^(-10) as an example (the square root is 10^(-5))
;; we see an error of about 10^(-3). That error is 100 times larger than
;; our goal:
(- (square (sqrt (expt 10 -10))) (expt 10 -10))
 ; => 9.76562466601563d-4 (0.09765624666015629d0%)

(defparameter good-enough-tolerance 0.1
  "percentage change to stop iteration")

(defun sqrt-1.7 (x)
  "calculate the square root by successive approximations"
  (let ((last 0))
    (flet ((good-enough? (guess)
	     (let ((change (abs (- guess last))))
	       (if (or (zerop change) (<= (/ guess change) good-enough-tolerance))
		   T
		   (progn
		     (setf last guess)
		     (<= (/ guess change) good-enough-tolerance))))
	   (improve (guess)
	     (average guess (/ x guess))))
      (labels ((sqrt-iter (guess)
		 (if (good-enough? guess)
		     guess
		     (sqrt-iter (improve guess)))))
	(sqrt-iter 1.0d0))))) ; use a double here.

(sqrt-1.7 2)
