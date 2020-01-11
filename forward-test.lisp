(in-package :forward)


(5am:def-suite forward-suite :description "The Forward Suite")
(5am:in-suite forward-suite)

(defvar foo 4)
(defvar test-env (make-env))
(defun run-str (str)
  (with-open-stream (s (make-string-input-stream str))
    (setf (env-stream test-env) s)
    (setf *exit-flag* nil)
    (handler-case
	(loop while (not *exit-flag*)
	   do
	     (let ((word (forth-read test-env)))
               (run word test-env)))
      (end-of-file (c)
	(declare (ignore c))))))

(defun clean-all (env)
  (setf (env-stream env) t)
  (setf (env-dictionary env) nil)
  (setf (env-stack env) nil)
  (init-dict env))
(defmacro runt (cmds)
  `(progn
     (setf test-env (make-env))
     (clean-all test-env)
     (run-str ,cmds)
     (env-stack test-env)))

(5am:test
 forward1
 (5am:is (equal '(2) (runt "1 1 +")))
 (5am:is (equal '(2 1) (runt "1 1 1 +")))
 (5am:is (equal '(3) (runt "1 1 1 + +")))
 (5am:is (equal '(8) (runt "2 2 2 + *")))
 (5am:is (equal '(8) (runt ": foo 2 2 + ; 2 foo *"))))

(5am:test
 tricky
 ;; This one test to make sure that redefining words still call the older one.
 ;; See hyperstatic environment.
 (5am:is (equal '(2 2) (runt ": foo 2 ; : bar foo ; bar : foo 4 ; bar")))
 ;; (5am:is (equal '(t) (runt "t if t then")))
 )
