(in-package :sicp)


;; *Exercise 1.24:* Modify the `timed-prime-test' procedure of *Note
;; Exercise 1-22:: to use `fast-prime?' (the Fermat method), and test
;; each of the 12 primes you found in that exercise.  Since the
;; Fermat test has [theta](`log' n) growth, how would you expect the
;; time to test primes near 1,000,000 to compare with the time needed
;; to test primes near 1000?  Do your data bear this out?  Can you
;; explain any discrepancy you find?


(defun timed-prime-test (n)
  (when (fast-prime? n (floor (log n)))
    (format t "~d : " n)
    (sicp-time (fast-prime? n (floor (log 100))))
    n))

(defun ex1.24 ()
  (search-for-primes 1000 3)
  (search-for-primes 10000 3)
  (search-for-primes 100000 3)
  (search-for-primes 1000000 3))

#|
(ex1.24)

SICP> (ex1.24)
1009 : processor-cycles: 9788
1013 : processor-cycles: 9404
1019 : processor-cycles: 9796
10007 : processor-cycles: 12664
10009 : processor-cycles: 37229
10037 : processor-cycles: 12147
100003 : processor-cycles: 14226
100019 : processor-cycles: 15213
100043 : processor-cycles: 15009
1000003 : processor-cycles: 17370
1000033 : processor-cycles: 17790
1000037 : processor-cycles: 17736

(log 1000)
6.9077554

(log 1000000)
13.815511

should be about double. That looks about right. The times for fermat test are of course a bit off.
Notably, the right primes were found. It's unclear from the text what an acceptable number of tests is for the check.
This is also about 4 times less than the tests in 1.23 for large n (and slower for small n)
|#
