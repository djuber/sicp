(in-package :sicp)

;; *Exercise 1.25:* Alyssa P. Hacker complains that we went to a lot
;; of extra work in writing `expmod'.  After all, she says, since we
;; already know how to compute exponentials, we could have simply
;; written

;; (define (expmod base exp m)
;;     (remainder (fast-expt base exp) m))

;; Is she correct?  Would this procedure serve as well for our fast
;; prime tester?  Explain.


(defun expmod* (base exp m)
  (mod (fast-expt base exp) m))

#|

This is clearly slower (increasing the size of the exponent is the best way to see that.

Sicp> (time ( expmod* 101 11234 41))
Evaluation took:
  0.001 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  2,268,696 processor cycles
  30,272 bytes consed

2 (2 bits, #x2, #o2, #b10)
SICP> (time ( expmod 101 11234 41))
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  9,555 processor cycles
  0 bytes consed

2 (2 bits, #x2, #o2, #b10)

|#
