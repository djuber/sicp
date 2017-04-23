(in-package :sicp)


;;    (4) [Footnote 1.47] Numbers that fool the Fermat test are called "Carmichael
;; numbers", and little is known about them other than that they are
;; extremely rare.  There are 255 Carmichael numbers below 100,000,000.
;; The smallest few are 561, 1105, 1729, 2465, 2821, and 6601.  In testing
;; primality of very large numbers chosen at random, the chance of
;; stumbling upon a value that fools the Fermat test is less than the
;; chance that cosmic radiation will cause the computer to make an error
;; in carrying out a "correct" algorithm.  Considering an algorithm to be
;; inadequate for the first reason but not for the second illustrates the
;; difference between mathematics and engineering.



;; *Exercise 1.27:* Demonstrate that the Carmichael numbers listed in
;; *Note Footnote 1-47:: really do fool the Fermat test.  That is,
;; write a procedure that takes an integer n and tests whether a^n is
;; congruent to a modulo n for every a<n, and try your procedure on
;; the given Carmichael numbers.

;; 561 fools the fermat test as described:
(prime? 561)
 ; => NIL
(fast-prime? 561 (floor (log 561)))
 ; => T

(smallest-divisor 561)
 ; => 3 (2 bits, #x3, #o3, #b11)

(defun carmichael-test (n)
  "check that a composite number fools the fermat test."
  (and (not (prime? n))
       (every (lambda (a)
		(= (mod a n)
		   (expmod a n n)))
	      (loop for i from 1 to (1- n) collect i))))



(defun ex1.27 ()
  (loop for i from 1 to 10000
	when (carmichael-test i)
	  collect (cons i (fast-prime? i (floor (log i))))))

(ex1.27)
 ; => ((561 . T) (1105 . T) (1729 . T) (2465 . T) (2821 . T) (6601 . T) (8911 . T))
