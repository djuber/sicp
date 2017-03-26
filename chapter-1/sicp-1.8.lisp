(in-package :sicp)
;; *Exercise 1.8:* Newton's method for cube roots is based on the
;; fact that if y is an approximation to the cube root of x, then a
;; better approximation is given by the value

;;      x/y^2 + 2y
;;      ----------
;;          3

;; Use this formula to implement a cube-root procedure analogous to
;; the square-root procedure.  (In section *Note 1-3-4:: we will see
;; how to implement Newton's method in general as an abstraction of
;; these square-root and cube-root procedures.)


;; just reuse the solution to 1.7, and replace "improve"
(defun cube-root (x)
  (let ((last 0))
    (flet ((good-enough? (guess)
	     (let* ((change (abs (- guess last)))
		    (relative (/ change guess)))
	       (if (or
		    (zerop change)
		    (<= relative good-enough-tolerance))
		   T
		   (prog1
		       nil
		     (setf last guess)))))
	   (improve (guess)
	     (/ (+
		 (/ x (square guess))
		 (* 2 guess))
		3)))
      (labels ((cube-root-iter (guess)
		 (if (good-enough? guess)
		     guess
		     (cube-root-iter (improve guess)))))
	(cube-root-iter 1.0d0)))))

(cube-root 3)
 ; => 1.4422497895989996d0 (144.22497895989997d0%)

(- (cube-root 3) (cl:expt 3 1/3))
 ; => 2.5308471740537186d-7 (2.5308471740537186d-5%)


