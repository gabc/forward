;;;; package.lisp

(defpackage #:forward
  (:use #:cl)
  (:export :new-env
	   :run-str
	   :load-file))
