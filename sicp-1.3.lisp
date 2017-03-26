(in-package :sicp)

     ;; *Exercise 1.3:* Define a procedure that takes three numbers as
     ;; arguments and returns the sum of the squares of the two larger
     ;; numbers.

(defun sum-of-squares-of-two-larger (a b c)
  ;; this is too easy if min/max are already in the environment
  ;; assuming only <, if, and basic math...
  (cond ((or (<= a b c) (<= a c b))  (sum-of-squares b c))
	((or (<= b c a) (<= b a c)) (sum-of-squares a c))
	(:else (sum-of-squares a b))))


