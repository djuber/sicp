(in-package :sicp)

;; *Exercise 1.22:* Most Lisp implementations include a primitive
;; called `runtime' that returns an integer that specifies the amount
;; of time the system has been running (measured, for example, in
;; microseconds).  The following `timed-prime-test' procedure, when
;; called with an integer n, prints n and checks to see if n is
;; prime.  If n is prime, the procedure prints three asterisks
;; followed by the amount of time used in performing the test.

(defun runtime ()
  (get-internal-run-time))

(defun timed-prime-test (n)
  ; (print n)
  (start-prime-test n (runtime)))

(defun start-prime-test (n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(defun report-prime (n elapsed-time)
  (format t   "~a  *** ~a~%" n elapsed-time)
  t)

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

(defun search-for-primes (start end &optional (count 0 count-supplied?))
  (if (evenp start)
      (search-for-primes (1+ start) end (if count-supplied? count))
      (let ((this 0))
	(do ((test start (+ 2 test)))
	    ((or (and count-supplied? count (= this count))
		 (> test end)))
	  (when (timed-prime-test test)
	    (incf this))))))

(search-for-primes 1000 1100 20)      
