(defpackage "SICP"
  (:use :cl)
  (:shadow
   cl:pi
   cl:abs
   )
  (:export
   :abs
   :area
   :circumference
   :pi
   :radius
   :size
   :square
   :sum-of-squares
   )
  (:documentation "SICP Solutions"))
  
(in-package :sicp)

;; 1.1.2 naming and the environment
(defconstant pi cl:pi)

(defparameter radius 10)

(defparameter size 2)

;; 1.1.4 compound procedures
(defun square (x) (* x x))

(defun circumference (radius)
  (* 2 pi radius))

(defun area (radius)
  (* pi (square radius)))

(defun sum-of-squares (x y)
  (+ (square x)
     (square y)))

(defun square-of-sum (x y)
  (square (+ x y)))

(defun f (a)
  (sum-of-squares (+ a 1) (* a 2)))

;; 1.1.5 substitution model for application

(defun abs (x) ; this, like <=, is predefined in Common Lisp
  (if (< x 0)
      (- x)
      x))

