(in-package :sicp)


;; *Exercise 1.22:* Most Lisp implementations include a primitive
;; called `runtime' that returns an integer that specifies the amount
;; of time the system has been running (measured, for example, in
;; 					       microseconds).  The following `timed-prime-test' procedure, when
;; called with an integer n, prints n and checks to see if n is
;; prime.  If n is prime, the procedure prints three asterisks
;; followed by the amount of time used in performing the test.

;; (define (timed-prime-test n)
;;     (newline)
;;   (display n)
;;   (start-prime-test n (runtime)))

;; (define (start-prime-test n start-time)
;;     (if (prime? n)
;; 	(report-prime (- (runtime) start-time))))

;; (define (report-prime elapsed-time)
;;     (display " *** ")
;;   (display elapsed-time))

;; Using this procedure, write a procedure `search-for-primes' that
;; checks the primality of consecutive odd integers in a specified
;; range.  Use your procedure to find the three smallest primes
;; larger than 1000; larger than 10,000; larger than 100,000; larger
;; than 1,000,000.  Note the time needed to test each prime.  Since
;; the testing algorithm has order of growth of [theta](_[sqrt]_(n)),
;; you should expect that testing for primes around 10,000 should
;; take about _[sqrt]_(10) times as long as testing for primes around
;; 1000.  Do your timing data bear this out?  How well do the data
;; for 100,000 and 1,000,000 support the _[sqrt]_(n) prediction?  Is
;; your result compatible with the notion that programs on your
;; machine run in time proportional to the number of steps required
;; for the computation?

;; make CL friendly... we're going to use the signature of sbcl's time macro, i.e. this will not compile for ccl
(defun sicp-print-time (&key real-time-ms user-run-time-us system-run-time-us gc-run-time-ms
		    processor-cycles eval-calls lambdas-converted page-faults bytes-consed aborted)
  (declare (ignore real-time-ms user-run-time-us system-run-time-us
		   gc-run-time-ms eval-calls eval-calls lambdas-converted page-faults bytes-consed aborted))
  (let ((*print-length* nil))
    (format t "processor-cycles: ~D~%" processor-cycles)))

(defmacro sicp-time (form)
  `(sb-ext:call-with-timing #'sicp::sicp-print-time (lambda () ,form)))

(defun timed-prime-test (n)
  (when (prime? n)
    (format t "~d : " n)
    (sicp-time (prime? n))
    n))

(defun search-for-primes (start count)
  (do ((candidate (if (oddp start) start (1+ start)) (+ candidate 2))
       (found 0))
      ((= found count))
    (if (timed-prime-test candidate)
	(incf found))))

(defun ex1.22 ()
  (search-for-primes 1000 3)
  (search-for-primes 10000 3)
  (search-for-primes 100000 3)
  (search-for-primes 1000000 3))

#|
SICP> (ex1.22)
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

This looks really close to (sqrt 10) => 3.162277665175675d0

The reason this perhaps doesn't run as slowly as expected is that the '3' next primes are
comparatively closer to the large numbers.
|#
