;;; attributes.lisp

(in-package #:influence)

(defclass renderable ()
  ())

(defmethod render ((this renderable)))

(defclass updateable ()
  ())

(defmethod update ((this updateable) dt))

