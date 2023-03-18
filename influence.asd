;;;; influence.asd

(asdf:defsystem #:influence
  :description "A game: claim territory on a map by placing influential tiles on the board"
  :author "Patrick Connelly psc23@protonmail.com"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
	       :cl-glfw3
	       :cl-opengl
	       :trivial-main-thread)
  :pathname #p "~/.roswell/local-projects/influence/"
  :components ((:file "package")
	       (:file "attributes")
	       (:file "util")
	       (:file "tilemap")               
	       (:file "influence")))
