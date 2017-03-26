(in-package :sicp)

     ;; *Exercise 1.2:* Translate the following expression into prefix
     ;; form.

     ;;      5 + 4 + (2 - (3 - (6 + 4/5)))
     ;;      -----------------------------
     ;;             3(6 - 2)(2 - 7)


(/
 (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
 (* 3 (- 6 2) (- 2 7)))
 ; => -37/150 (-0.24666667, -74/3%)

;; as a check, ran that above in python to verify
#|
( 5 + 4 + (2 - (3 - (6 + 0.8))))/(3 * (6 -2 ) * (2 - 7))
-0.24666666666666667
|#
