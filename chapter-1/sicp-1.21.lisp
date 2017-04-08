(in-package :sicp)


;; *Exercise 1.21:* Use the `smallest-divisor' procedure to find the
;; smallest divisor of each of the following numbers: 199, 1999,
;; 19999.

(loop for number in (list 199 1999 19999) collect (smallest-divisor number))
 ; => (199 1999 7)

(prime? 199)
 ; => T

(prime? 1999)
 ; => T

(prime? 19999)
 ; => NIL
