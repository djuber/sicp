(in-package :sicp)


;; *Exercise 1.15:* The sine of an angle (specified in radians) can
;; be computed by making use of the approximation `sin' xapprox x if
;; x is sufficiently small, and the trigonometric identity

;;                     x             x
;;      sin x = 3 sin --- - 4 sin^3 ---
;;                     3             3

;; to reduce the size of the argument of `sin'.  (For purposes of this
;; exercise an angle is considered "sufficiently small" if its
;; magnitude is not greater than 0.1 radians.) These ideas are
;; incorporated in the following procedures:

;;      (define (cube x) (* x x x))

;;      (define (p x) (- (* 3 x) (* 4 (cube x))))

;;      (define (sine angle)
;;         (if (not (> (abs angle) 0.1))
;;             angle
;;             (p (sine (/ angle 3.0)))))


(defun cube (x) (* x x x))

(defun p (x) (- (* 3 x) (* 4 (cube x))))

(defun sine (angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; (trace p)
;; (sine 12.15)

#|
  0: (P 0.049999997)
  0: P returned 0.1495
  0: (P 0.1495)
  0: P returned 0.43513453
  0: (P 0.43513453)
  0: P returned 0.9758465
  0: (P 0.9758465)
  0: P returned -0.7895632
  0: (P -0.7895632)
  0: P returned -0.39980328
|#

;; a) p is called 5 times. as sine 12.15 expands to (p (sine 4.05)) -> (p (p (sine 1.34))) -> (p (p (p (since 0.444)))) -> (p (p (p (p (sine 0.1444))))) -> (p (p (p (p (p (sine 0.0499997))))))

; (sin 12.15)
 ; => -0.40444416 (-40.444416%)

;(sine 12.15)
 ; => -0.39980328 (-39.980328%)

;; b) what is the order of growth in space and time?

;; Space is directly related to the number of calls to sine. Time and space are logarithmic.


;; (labels ((order (x count)
;; 	   (if (< x 0.1)
;; 	       count
;; 	       (order (/ x 3.0) (1+ count)))))
;;   (order 200000 0))
