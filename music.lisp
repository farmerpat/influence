;;; music.lisp

;;; what if we could say
(definstrument trumpet :sample #p"trumpet.ogg")

(repeat
 (phrase
  (trumpet
   (eight-bars-of
   ;; bar of sixteenths...barw for bar of whole?
   ;; or would we rather 
    (bars
     ;; great opportunity for a pattern language...
     ;; how do we describe notes? '+--+ ?
     ;; perhpas +--+ e.g. is a macro.
     ;; perhaps every permutation of such sixteenth note patterns
     ;; is created automatically by another macro.
     ;; or would we rather say 8-bars-of and then the four beat specs
     ;; in a row? Or perhaps just the beat spec symbols themselves all in a row e.g.
     ;; (trumpet (8-bars-of +--- --+- +--- ----) (2-bars-of ...)

     (beat +---)
     (beat --+-)
     (beat +---)
     (beat ----)
     )))))

;; seems slow...
(phrase
 (trumpet
  (8-bars-of
   (+---) (--+-) (+---) (----)))
 (bass-drum
  (8-bars-of
   (+-+- --+- +--- ----)))
 (snare-drum
  (8-bars-of
   (---- ---- ---- +---))))


(ql:quickload :cl-patterns/supercollider)
(cl-patterns:backend-start 'supercollider)



