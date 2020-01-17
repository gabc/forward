(in-package :forward)


(5am:def-suite forward-suite :description "The Forward Suite")
(5am:in-suite forward-suite)

(defvar foo 4)
(defvar test-env (make-env))
(defun run-str (str)
  (with-open-stream (s (make-string-input-stream str))
    (setf (env-stream test-env) s)
    (setf *exit-flag* nil)
    (let (words)
      (handler-case
	  (loop while (not *exit-flag*)
	     do
	       (let ((word (forth-read test-env)))
		 (push word words)))
	(end-of-file (c)
	  (declare (ignore c))))
	(run (reverse words) test-env))))

(defun runt (cmds)
  (env-stack (runte cmds)))
(defmacro runte (cmds)
  `(progn
     (setf test-env (make-env))
     (clean-all test-env)
     (run-str ,cmds)
     test-env))

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
 (5am:is (equal '(4 2) (runt ": foo 2 ; : bar foo ; bar : foo 4 ; bar")))
 (5am:is (equal '(1 1) (runt ": a 1 ; : b a ; : c b ; : d c ; : e d ; a e")))
 (5am:is (equal '(3) (runt "2 skip 1 2 3")))
 (5am:is (equal '(0 0) (runt ": foo dup 0 = if 0 else 1 - rec then ; 4 foo")))
 (5am:is (equal '(3) (runt ": foo skip 1 2 3 ; 2 foo")))
 (5am:is (equal '(3) (runt ": foo 2 skip 1 2 3 ; foo"))))

(5am:test
 ifs
 (5am:is (equal '(2) (runt ": foo if 2 else 10 then ; t foo")))
 (5am:is (equal '(10) (runt ": foo if 2 else 10 then ; nil foo")))
 (5am:is (equal '(1) (runt ": foo if 1 else 2 then ; : bar foo ; t bar")))
 (5am:is (equal '(2) (runt ": foo if 1 else 2 then ; : bar foo ; nil bar")))
 (5am:is (equal '(22) (runt ": bar if 22 else 33 then ; : foo if bar else 10 then ; t t foo"))))

(defun at ()
  (5am:run! '(tricky forward1 ifs)))
