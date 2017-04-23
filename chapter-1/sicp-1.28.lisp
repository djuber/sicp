(in-package :sicp)

;; *Exercise 1.28:* One variant of the Fermat test that cannot be
;; fooled is called the "Miller-Rabin test" (Miller 1976; Rabin
;; 1980).  This starts from an alternate form of Fermat's Little
;; Theorem, which states that if n is a prime number and a is any
;; positive integer less than n, then a raised to the (n - 1)st power
;; is congruent to 1 modulo n.  To test the primality of a number n
;; by the Miller-Rabin test, we pick a random number a<n and raise a
;; to the (n - 1)st power modulo n using the `expmod' procedure.
;; However, whenever we perform the squaring step in `expmod', we
;; check to see if we have discovered a "nontrivial square root of 1
;; modulo n," that is, a number not equal to 1 or n - 1 whose square
;; is equal to 1 modulo n.  It is possible to prove that if such a
;; nontrivial square root of 1 exists, then n is not prime.  It is
;; also possible to prove that if n is an odd number that is not
;; prime, then, for at least half the numbers a<n, computing a^(n-1)
;; in this way will reveal a nontrivial square root of 1 modulo n.
;; (This is why the Miller-Rabin test cannot be fooled.)  Modify the
;; `expmod' procedure to signal if it discovers a nontrivial square
;; root of 1, and use this to implement the Miller-Rabin test with a
;; procedure analogous to `fermat-test'.  Check your procedure by
;; testing various known primes and non-primes.  Hint: One convenient
;; way to make `expmod' signal is to have it return 0.


(defun m-r-expmod (base expt modulus)
  (let ((trivial-square-roots (list 1 (1- modulus))))
  (flet ((nontrivial (n)
	   (and (not (member base trivial-square-roots)) ; base is a square root, but not a trivial square root of 1
		(= 1 (mod (square n) modulus)))))
    (cond ((= expt 0) 1)
	  ((evenp expt)
	   (if (nontrivial base)
	       0
	       (mod (square (m-r-expmod base (/ expt 2) modulus))
		    modulus)))
	  (:else (mod (* base (m-r-expmod base (- expt 1) modulus))
		      modulus))))))

(defun miller-rabin-prime (n tries)
   (reduce (lambda (x y) (and x y)) (loop for i upto tries collect (miller-rabin-test n)) :initial-value t))

(defun miller-rabin-test (n)
  (let ((test (m-r-expmod (random (1- n)) (1- n) n)))
    (= 1 test)))

(loop for i from 3 to 1999 by 2
      when  (miller-rabin-prime i (ceiling (sqrt i)))
	when   (prime? i)
	  collect i)

;; so this is close to right...
;; we never misidentify a composite as prime, i.e. we can rule out a
;; candidate as soon as it fails.

(defun good-prime? (n)
  (if (< n 1000)
      (prime? n)
      (miller-rabin-test n)))

;; what happens however is that we fail to consistently find all the primes this way.
