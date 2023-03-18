;;;; influence.lisp

;;; Play could be turn-based or could maintain count of playable moves that
;;; replenishes over time once spent. Start with 6 perhaps...
;;; experiment with both ftw.

(in-package #:influence)


(defparameter *window-width* 1024)
(defparameter *window-height* 768)

(defparameter *gobs* nil)
(defparameter *dt* 0.0)
(defparameter *last-time* 0.0)

(defparameter *mouse-pos* (make-point -1.0 -1.0))
(defparameter *temp* nil)
(defparameter *temp2* nil)
(defparameter *mouse-x* nil)
(defparameter *mouse-y* nil)

(defparameter *tm* (generate-neutral-tile-map *tile-map-width* *tile-map-height*))

(defun update-all ()
  (let* ((now (get-time))
	 (dt (* 1000 (- now *last-time*))))
    (loop for gob in *gobs*
	  do
 	     (when (typep gob 'updateable)
	       (update gob dt)))
    (setf *last-time* now)))

(defun render-all ()
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:rect 0 0 23 23)
    (loop for gob in *gobs*
	  do
 	     (when (typep gob 'renderable)
	       (render gob)))))

;; TODO: do we want a container class?
;; do we want a helper atleast e.g. push-gob
;;
;; TODO: also how to remove? there was some (delete) fn used somewhere by some lib...crib that shit
;;(push (make-instance 'tile :pos (make-point 16 16) :color (make-color 0.5 0.5 0.5)) *gobs*)
(push *tm* *gobs*)

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0.0 *window-width* *window-height* 0.0 1.0 0.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(def-key-callback kbd-handler (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close))
  ;; (when (and (eq key :space) (eq action :press))
  ;; 	(setf *tm* (generate-neutral-tile-map *tile-map-width* *tile-map-height*)))
  )

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window w h))
  ;; b/c :resizable nil seems to be ignored/unimplemened/unimplamentable/whatever.
  (set-window-size *window-width* *window-height*)
  (set-viewport *window-width* *window-height*))

(def-cursor-pos-callback cursor-callback (window x y)
  (declare (ignore window))
  (setf (car *mouse-pos*) x)
  (setf (cdr *mouse-pos*) y))

(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys window))
  (when (eq action :press)
    (let ((map-coords (screen-point->map-point *mouse-pos*)))
      ;; test if the coords are in the bounds of the map!
      (cond ((eq button :left)
	     ;; TODO: but should determining in-bounds be the responsibility of this piece of code?
	     ;; TODO: we shouldn't have to be swapping to counter-intuitive array indices
	     ;; at this level...fix the design.
	     (when (in-bounds *tm* (point-x map-coords) (point-y map-coords))
	       (setf (transition-state (tile-at *tm* (point-x map-coords) (point-y map-coords)))
		     :lightening)))
	    ((eq button :right)
	     (when (in-bounds *tm* (point-x map-coords) (point-y map-coords))
	       (setf (transition-state (tile-at *tm* (point-x map-coords) (point-y map-coords)))
		     :darkening)))))))

(defun run ()
  (with-body-in-main-thread ()
    (with-init-window (:title "Hello, Earl!"
		       :width *window-width*
		       :height *window-height*
		       :resizable nil)
      (setf *last-time* (get-time))
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'kbd-handler)
      (set-window-size-callback 'update-viewport)
      (set-mouse-button-callback 'mouse-callback)
      (set-cursor-position-callback 'cursor-callback)
      (gl:clear-color 0 0 0 0)
      (set-viewport *window-width* *window-height*)
      (loop until (window-should-close-p)
	    do (update-all)
	    do (render-all)
	    do (swap-buffers)
	    do (poll-events)))))
