;;; util.lisp

(in-package #:influence)

(defun make-point (x y)
  (cons x y))

(defun point-x (p)
  (car p))

(defun point-y (p)
  (cdr p))

(defun make-color (r g b &optional (a 1.0))
  (list (float r) (float g) (float b) (float a)))

(defun color-r (c) (first c))
(defun color-g (c) (second c))
(defun color-b (c) (third c))
(defun color-a (c) (fourth c))

(defun color-dark-light-percent (c)
  "input: <color> c
   output: (values %-light %-dark)"
  ;; TODO
  ;; for now assume r g b will all be equal. e.g. black&white
  (cond ((= 0.5 (color-r c))
	 (values 0.5 0.5))
	((> (color-r c) 0.5)
	 (let ((lightness (/ (color-r c) 1.0)))
	   (values lightness
		   (- 1.0 lightness))))
	(t
	 (let ((darkness (/ (- 1 (color-r c)) 1.0)))
	   (values (- 1.0 darkness)
		   darkness)))))

(defun lighten-color (c amount)
  (let ((r (clamp (+ (color-r c) amount) 0.0 1.0))
	(g (clamp (+ (color-g c) amount) 0.0 1.0))
	(b (clamp (+ (color-b c) amount) 0.0 1.0)))
    (make-color r g b (color-a c))))

(defun darken-color (c amount)
  (let ((r (clamp (- (color-r c) amount) 0.0 1.0))
	(g (clamp (- (color-g c) amount) 0.0 1.0))
	(b (clamp (- (color-b c) amount) 0.0 1.0)))
    (make-color r g b (color-a c))))
