(in-package :sicp)


;; *Exercise 1.21:* Use the `smallest-divisor' procedure to find the
;; smallest divisor of each of the following numbers: 199, 1999,
;; 19999.

(loop for i in (list 199 1999 19999)
      collect (smallest-divisor i))
 ; => (199 1999 7)

