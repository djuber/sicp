(in-package :sicp)
     ;; *Exercise 1.4:* Observe that our model of evaluation allows for
     ;; combinations whose operators are compound expressions.  Use this
     ;; observation to describe the behavior of the following procedure:

     ;;      (define (a-plus-abs-b a b)
     ;;        ((if (> b 0) + -) a b))

;;; this code checks if b is positive, and returns the  "+" procedure if so
;;; or the "-" procedure if not. It then applies the returned procedure
;;; to a and b.

;; the lisp way

(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b))

(a-plus-abs-b 10 -4)
 ; => 14 (4 bits, #xE, #o16, #b1110)

