(in-package :sicp)


;; *Exercise 1.23:* The `smallest-divisor' procedure shown at the
;; start of this section does lots of needless testing: After it
;; checks to see if the number is divisible by 2 there is no point in
;; checking to see if it is divisible by any larger even numbers.
;; This suggests that the values used for `test-divisor' should not
;; be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, ....  To
;; implement this change, define a procedure `next' that returns 3 if
;; its input is equal to 2 and otherwise returns its input plus 2.
;; Modify the `smallest-divisor' procedure to use `(next
;; test-divisor)' instead of `(+ test-divisor 1)'.  With
;; `timed-prime-test' incorporating this modified version of
;; `smallest-divisor', run the test for each of the 12 primes found in
;; *Note Exercise 1-22::.  Since this modification halves the number
;; of test steps, you should expect it to run about twice as fast.
;; Is this expectation confirmed?  If not, what is the observed ratio
;; of the speeds of the two algorithms, and how do you explain the
;; fact that it is different from 2?

;; redefining smallest-divisor
(defun smallest-divisor (n)
  (flet ((next-divisor (divisor)
	     (if (evenp divisor) (1+ divisor) (+ divisor 2))))
    (labels ((find-divisor (test)
	       (cond
		 ((> (square test) n) n)
		 ((divides? test n) test)
		 (:else (find-divisor (next-divisor test))))))
      (find-divisor 2))))


(defun prime? (n)
  "this is a faster way to do it, since smallest-divisor is better."
  (= n (smallest-divisor n)))

#|
(ex1.22) ; using above definitions
1009 : processor-cycles: 2620
1013 : processor-cycles: 1824
1019 : processor-cycles: 1780
10007 : processor-cycles: 4796
10009 : processor-cycles: 4780
10037 : processor-cycles: 4780
100003 : processor-cycles: 96104
100019 : processor-cycles: 14283
100043 : processor-cycles: 14103
1000003 : processor-cycles: 44316
1000033 : processor-cycles: 43932
1000037 : processor-cycles: 43578



(ex1.22) ; using 1.22 and chapter definitions
1009 : processor-cycles: 3666
1013 : processor-cycles: 3132
1019 : processor-cycles: 3096
10007 : processor-cycles: 9240
10009 : processor-cycles: 9240
10037 : processor-cycles: 9264
100003 : processor-cycles: 28665
100019 : processor-cycles: 28908
100043 : processor-cycles: 28629
1000003 : processor-cycles: 89004
1000033 : processor-cycles: 90141
1000037 : processor-cycles: 90198


This is very close to 2:1, to wit

(/ 3096 1780.0)
1.7393259 (173.93259%)

(/ 9264 4780.0)
1.9380753 (193.80753%)

(/ 28629 14103.0)
2.0299935

(/ 90198 43578.0)
2.0698059
|#
