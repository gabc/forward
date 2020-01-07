;;;; forward.asd

(asdf:defsystem #:forward
  :description "Describe forward here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:log4cl :fiveam)
  :components ((:file "package")
               (:file "forward")
	       (:file "forward-test")))

