(in-package :sicp)

     ;; *Exercise 1.5:* Ben Bitdiddle has invented a test to determine
     ;; whether the interpreter he is faced with is using
     ;; applicative-order evaluation or normal-order evaluation.  He
     ;; defines the following two procedures:

     ;;      (define (p) (p))

     ;;      (define (test x y)
     ;;        (if (= x 0)
     ;;            0
     ;;            y))

     ;; Then he evaluates the expression

     ;;      (test 0 (p))

     ;; What behavior will Ben observe with an interpreter that uses
     ;; applicative-order evaluation?  What behavior will he observe with
     ;; an interpreter that uses normal-order evaluation?  Explain your
     ;; answer.  (Assume that the evaluation rule for the special form
     ;; `if' is the same whether the interpreter is using normal or
     ;; applicative order: The predicate expression is evaluated first,
     ;; and the result determines whether to evaluate the consequent or
     ;; the alternative expression.)

(defun p ()
  (p))

(defun test (x y)
  (if (zerop x)
      0
      y))

;; in common lisp this runs for a long time (p) is called first:
(trace p)
(test 0 (p))

;; I got bored when this scrolled down past 15,000

;; in any case, in 'applicative' order, the call to p is made. In
;; normal order, the arguments are passed to test unevaluated.

;; in a lazy language, we would get 0. In lisp, you can make test a macro

(defmacro test1 (x y)
  `(if (zerop ,x)
       0
       ,y))

(test1 0 (p))
 ; => 0 (0 bits, #x0, #o0, #b0)

;; since we use a macro, the object (list 'p) is passed as y, and
;; if does not evaluate the alternate when the condition is true.

;; we can get the old behavior by running test1 with a non-zero x:
(test1 1 (p))

;; Control stack exhausted (no more space for function call frames).
;; This is probably due to heavily nested or infinitely recursive function
;; calls, or a tail call that SBCL cannot or has not optimized away.

;; PROCEED WITH CAUTION.
;;    [Condition of type SB-KERNEL::CONTROL-STACK-EXHAUSTED]
