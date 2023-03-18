;;; tileset.lisp

(in-package #:influence)

(defparameter *tile-width* 16)
(defparameter *tile-height* 16)
;; 64x48 should be fullscreen @ 1024x768
(defparameter *tile-map-width* 63)
(defparameter *tile-map-height* 47)
(defparameter *tile-map-tile-transition-states*
  (list :lightening :darkening :inactive))

(defun map-pos->left-neighbor-pos (pos)
  (make-point (1- (point-x pos)) (point-y pos)))

(defun map-pos->right-neighbor-pos (pos)
  (make-point (1+ (point-x pos)) (point-y pos)))

(defun map-pos->upper-neighbor-pos (pos)
  (make-point (point-x pos) (1- (point-y pos))))

(defun map-pos->lower-neighbor-pos (pos)
  (make-point (point-x pos) (1+ (point-y pos))))

(defun map-pos->upper-left-neighbor-pos (pos)
  (make-point (1- (point-x pos)) (1- (point-y pos))))

(defun map-pos->upper-right-neighbor-pos (pos)
  (make-point (1+ (point-x pos)) (1- (point-y pos))))

(defun map-pos->lower-left-neighbor-pos (pos)
  (make-point (1- (point-x pos)) (1+ (point-y pos))))

(defun map-pos->lower-right-neighbor-pos (pos)
  (make-point (1+ (point-x pos)) (1+ (point-y pos))))

;; We need a way to ask of a tile: how influenced is it by its neighbors
;; and in what direction?
;; e.g. if its neighbors are mostly white, how white are they?
;; then how stronly should they influence it?
;; we need a way to map strength of light/dark influence to time.

;; well... all the neighbors have 800% between them...
;; could perhaps add all white% and divide by 8 etc?

(defclass tile-map (renderable updateable)
  ((tiles :initarg :tiles :accessor tiles)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (screen-origin :initarg :screen-origin :accessor screen-origin :initform (make-point 0 0))))

;; TODO: perhaps callers will manipulate transition-interval, transition-amount or
;; transition-counter in response to this information and/or switch transition-state
;; e.g. if we receive 0.73999995 0.26000008 as return values...
;; then the state should be lightening...
;; perhaps it should just flip state or set as neutral if ~=50%?
;; then what?
;; TODO: there has to be a better way.
(defmethod compute-neighbor-influence ((this tile-map) x y)
  "input: tile-map, x, y
   returns: (values total-light-percent total-dark-percent"
  (let* ((mp (make-point x y))
	 (left-pos (map-pos->left-neighbor-pos mp))
	 (right-pos (map-pos->right-neighbor-pos mp))
	 (upper-pos (map-pos->upper-neighbor-pos mp))
	 (lower-pos (map-pos->lower-neighbor-pos mp))
	 (upper-left-pos (map-pos->upper-left-neighbor-pos mp))
	 (upper-right-pos (map-pos->upper-right-neighbor-pos mp))
	 (lower-left-pos (map-pos->lower-left-neighbor-pos mp))
	 (lower-right-pos (map-pos->lower-right-neighbor-pos mp))
	 (left-neighbor (tile-at this (point-x left-pos) (point-y left-pos)))
	 (right-neighbor (tile-at this (point-x right-pos) (point-y right-pos)))
	 (upper-neighbor (tile-at this (point-x upper-pos) (point-y upper-pos)))
	 (lower-neighbor (tile-at this (point-x lower-pos) (point-y lower-pos)))
	 (upper-left-neighbor (tile-at this (point-x upper-left-pos) (point-y upper-left-pos)))
	 (upper-right-neighbor (tile-at this (point-x upper-right-pos) (point-y upper-right-pos)))
	 (lower-left-neighbor (tile-at this (point-x lower-left-pos) (point-y lower-left-pos)))
	 (lower-right-neighbor (tile-at this (point-x lower-right-pos) (point-y lower-right-pos)))
	 (neighbors (list left-neighbor
			  right-neighbor
			  upper-neighbor
			  lower-neighbor
			  upper-left-neighbor
			  upper-right-neighbor
			  lower-left-neighbor
			  lower-right-neighbor))
	 (light-total 0.0)
	 (dark-total 0.0)
	 (tile-total 0.0))

    (dolist (neighbor neighbors)
      (when neighbor
	(incf tile-total 1.0)
	(multiple-value-bind (lightness darkness)
	    (color-dark-light-percent (color neighbor))
	  (incf light-total lightness)
	  (incf dark-total darkness))))

    (let ((total-light-percent (/ light-total tile-total))
	  (total-dark-percent (/ dark-total tile-total)))
      (values total-light-percent
	      total-dark-percent))))

(defparameter *peer-pressure-threashold* 0.7)
;; TODO flip a coin to see who's threshold
;; gets checked first.
;; this might have incredible results....
(setf *peer-pressure-threashold* 0.6)

(defmethod update ((this tile-map) dt)
  ;; TODO:
  ;; okay...major question. We want tiles to affect their neighbors.
  ;; do we want to leave the calculations to tile-map or to tile-map-tile?
  ;; fundamentally: should tile-map-tiles know about their own position
  ;; within the tile-map?
  ;; if they know their own positions, then they can...
  ;; but they'd have to know about the tile-map-width etc...
  ;; suppose it must be left to the tile-map itself...
  ;; it knows about all of it's things...

  ;; first impose influence of neighbors on all tiles
  (loop for x from 0 to (1- (width this))
	do
	   (loop for y from 0 to (1- (height this))
		 do
		    ;;(when (and (= x 0) (= y 0)))
		    (multiple-value-bind (light-percent dark-percent)
			(compute-neighbor-influence this x y)
		      (when (not (= light-percent 0.5))
			(let ((this-tile (tile-at this x y)))
			  (if (zerop (random 2))
			    (if (> light-percent *peer-pressure-threashold*)
			      (case (transition-state this-tile)
				((:inactive)
				 (setf (transition-state this-tile) :lightening))
				((:darkening)
				 (setf (transition-state this-tile) :lightening)))
			      (if (> dark-percent *peer-pressure-threashold*)
				  (case (transition-state this-tile)
				    ((:inactive)
				     (setf (transition-state this-tile) :darkening))
				    ((:lightening)
				     (setf (transition-state this-tile) :darkening)))))
			    (if (> dark-percent *peer-pressure-threashold*)
				(case (transition-state this-tile)
				    ((:inactive)
				     (setf (transition-state this-tile) :darkening))
				    ((:lightening)
				     (setf (transition-state this-tile) :darkening)))
				(if (> light-percent *peer-pressure-threashold*)
				    (case (transition-state this-tile)
				      ((:inactive)
				       (setf (transition-state this-tile) :lightening))
				      ((:darkening)
				       (setf (transition-state this-tile) :lightening)))))))))))

  (loop for x from 0 to (1- (width this))
	do
	   (loop for y from 0 to (1- (height this))
		 do
		    (update (tile-at this x y) dt))))

;; TODO: should probably be a point object instead of x,y
(defmethod in-bounds ((this tile-map) x y)
  (array-in-bounds-p (tiles this) y x))

(defmethod tile-at ((this tile-map) x y)
  (when (in-bounds this x y)
    (aref (tiles this) y x)))

;; (defmethod tile-at ((this tile-map) p)
;;   ;; TODO point? should exist...or make point into a class for type checking
;;   (tile-at this (point-x p) (point-y p)))

;; TODO: I think this is meant to ref y,x NOT x,y...
;; sort it out: decide and move forward.
;; asap: move above this abstraction layer to avoid thinking about it at all.
(defmethod set-tile-at ((this tile-map) x y value)
  (when (in-bounds this x y)
    (setf (aref (tiles this) y x) value)))

(defmethod render ((this tile-map))
  ;; TODO: it might be nicer to just call render on all the tiles...
  (loop for x from 0 to (1- (width this))
	do
	   (loop for y from 0 to (1- (height this))
		 do
		    (let* ((tile (tile-at this x y))
			   (c (color tile))
			   (p (make-point (* *tile-width* x) (* *tile-height* y))))
		      (gl:color (color-r c)
				(color-g c)
				(color-b c)
				(color-a c))
		      ;; x1 y1 x2 y2
		      (gl:rect (point-x p)
			       (point-y p)
			       (+ (point-x p) *tile-width*)
			       (+ (point-y p) *tile-height*))))))

(defun generate-neutral-tiles (w h)
  (let ((tm-tiles
	  (make-array `(,h ,w) :initial-element nil)))
    (loop for row from 0 to (1- h)
	  do
	     (loop for col from 0 to (1- w)
		   do
		      (let ((this-tile
			      (make-tile-map-tile
			       (make-point col row)
			       ;; (if (zerop (random 2))
			       ;; 	   (make-color 0.0 0.0 0.0)
			       ;; 	   (make-color 0.5 0.5 0.5))
			       (make-color 0.5 0.5 0.5)
			       *tile-width*
			       *tile-height*)))
			(setf (aref tm-tiles row col) this-tile))))
    tm-tiles))

(defun generate-neutral-tile-map (w h)
  (make-instance 'tile-map
                 :tiles (generate-neutral-tiles w h)
                 :width w
                 :height h))

(defclass tile-map-tile (updateable)
  ((pos
    :initform (make-point 0 0)
    :initarg :pos
    :accessor pos)
   (width :initform 16 :initarg :width :accessor width)
   (height :initform 16 :initarg :height :accessor height)
   (transition-counter :initform 0.0 :accessor transition-counter)
   (transition-interval :initform 100.0 :accessor transition-interval :initarg :transition-interval)
   (transition-state :initform :inactive :accessor transition-state)
   (transition-amount :initform 0.01 :accessor transition-amount :initarg :transition-amount)
   (color :initform (make-color 0.5 0.5 0.5 1.0)
	  :initarg :color
	  :accessor color)))

(defun make-tile-map-tile (p c &optional (w 16) (h 16))
  (make-instance 'tile-map-tile :pos p :width w :height h :color c))

(defmethod transition-color ((this tile-map-tile))
  (when (not (eq (transition-state this) :inactive))
    (if (eq (transition-state this) :lightening)
	(setf (color this) (lighten-color (color this) (transition-amount this)))
	(setf (color this) (darken-color (color this) (transition-amount this))))))

(defmethod update ((this tile-map-tile) dt)
  (incf (transition-counter this) dt)
  ;; if transition-counter > transition-interval,
  ;; reset transition-counter and put in work.
  (when (> (transition-counter this) (transition-interval this))
    (when (not (eq (transition-state this) :inactive))
      ;; bloody transition it...
      (transition-color this))
    (setf (transition-counter this) 0.0)))

(defun screen-point->map-point (p)
  (let ((x (point-x p))
	(y (point-y p)))
    (make-point
     (floor (/ (float x) *tile-width*))
     (floor (/ (float y) *tile-height*)))))


;;; TODO: Probably throw this away because it isn't necessary.
(defclass tile (renderable)
  ;; TODO: seems like position is taken...am I duplicating work here
  ((pos :initform (make-point 0 0)
	:initarg :pos
	:accessor pos)
   (width :initform 16 :initarg :width :accessor width)
   (height :initform 16 :initarg :height :accessor height)
   (color :initform (make-color 1.0 1.0 1.0 1.0)
	  :initarg :color
	  :accessor color)))

(defun make-tile (p c &optional (w 16) (h 16))
  (make-instance 'tile :pos p :color c :width w :height h))

(defmethod render ((this tile))
  ;; TODO: I think I want with-slots...
  (let ((c (color this))
	(p (pos this)))
    (gl:color (color-r c)
	      (color-g c)
	      (color-b c)
	      (color-a c))
    ;; x1 y1 x2 y2
    (gl:rect (point-x p)
	     (point-y p)
	     (+ (point-x p) (width this))
	     (+ (point-y p) (height this)))))

