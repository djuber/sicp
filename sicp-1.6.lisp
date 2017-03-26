(in-package :sicp)

;; *Exercise 1.6:* Alyssa P. Hacker doesn't see why `if' needs to be
;;      provided as a special form.  "Why can't I just define it as an
;;      ordinary procedure in terms of `cond'?" she asks.  Alyssa's friend
;;      Eva Lu Ator claims this can indeed be done, and she defines a new
;;      version of `if':

;;           (define (new-if predicate then-clause else-clause)
;;             (cond (predicate then-clause)
;;                   (else else-clause)))

;;      Eva demonstrates the program for Alyssa:

;;           (new-if (= 2 3) 0 5)
;;           5

;;           (new-if (= 1 1) 0 5)
;;           0

;;      Delighted, Alyssa uses `new-if' to rewrite the square-root program:

;;           (define (sqrt-iter guess x)
;;             (new-if (good-enough? guess x)
;;                     guess
;;                     (sqrt-iter (improve guess x)
;;                                x)))

;;      What happens when Alyssa attempts to use this to compute square
;;      roots?  Explain.

(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
	(:else else-clause)))

(new-if (= 2 3) 0 5)
 ; => 5 (3 bits, #x5, #o5, #b101)

(new-if (= 1 1) 0 5)
 ; => 0 (0 bits, #x0, #o0, #b0)

;; again, applicative order will defeat this. Lets play!

(defun broken-sqrt-iter (guess x)
  (flet ((improve (guess x) guess))
    (new-if T
	guess
	(broken-sqrt-iter (improve guess x) x))))
			   

(trace broken-sqrt-iter)
 ; => (BROKEN-SQRT-ITER)

(broken-sqrt-iter 10 100)

;;                             15783: (BROKEN-SQRT-ITER 10 100)
;;                               15784: (BROKEN-SQRT-ITER 10 100)
;;                                 15785: (BROKEN-SQRT-ITER 10 100)
;;                                   15786: (BROKEN-SQRT-ITER 10 100)
;;                                     15787: (BROKEN-SQRT-ITER 10 100)
;;                                       15788: (BROKEN-SQRT-ITER 10 100)
;;   15789: (BROKEN-SQRT-ITER 10 100)
;;     15790: (BROKEN-SQRT-ITER 10 100)
;;       15791: (BROKEN-SQRT-ITER 10 100)
;; Control stack guard page temporarily disabled: proceed with caution



;; Since new-if is a function, it's arguments are evaluated when called.
;; this exercise exists to further drive home the 'application' part
;; of applicative order.
