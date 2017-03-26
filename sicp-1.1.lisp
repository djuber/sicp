(in-package :sicp)

 
     ;; *Exercise 1.1:* Below is a sequence of expressions.  What is the
     ;; result printed by the interpreter in response to each expression?
     ;; Assume that the sequence is to be evaluated in the order in which
     ;; it is presented.


;; emacs note - used "M-x global-set-key RET C-c C-j RET sly-eval-print-last-expression"

;; cl- change define to defvar:

          10
 ; => 10 (4 bits, #xA, #o12, #b1010)

          (+ 5 3 4)

 ; => 12 (4 bits, #xC, #o14, #b1100)
          (- 9 1)

 ; => 8 (4 bits, #x8, #o10, #b1000)
          (/ 6 2)
 ; => 3 (2 bits, #x3, #o3, #b11)

          (+ (* 2 4) (- 4 6))
 ; => 6 (3 bits, #x6, #o6, #b110)

          (defvar a 3)
 ; => A


          (defvar b (+ a 1))
 ; => B

          (+ a b (* a b))
 ; => 19 (5 bits, #x13, #o23, #b10011)

          (= a b)
 ; => NIL

          (if (and (> b a) (< b (* a b)))
              b
              a)
; => 4 (3 bits, #x4, #o4, #b100)


          (cond ((= a 4) 6)
                ((= b 4) (+ 6 7 a))
                (else 25))

 ; => 16 (5 bits, #x10, #o20, #b10000)
          (+ 2 (if (> b a) b a))
 ; => 6 (3 bits, #x6, #o6, #b110)

          (* (cond ((> a b) a)
                   ((< a b) b)
                   (else -1))
             (+ a 1))
 ; => 16 (5 bits, #x10, #o20, #b10000)

