;;;; package.lisp

(defpackage #:forward
  (:use #:cl)
  (:nicknames :fw)
  (:export :new-env
	   :run-str
	   :load-file
	   :env-variables
	   :env-stack
	   :env-rstack))
