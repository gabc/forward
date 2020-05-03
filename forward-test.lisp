(in-package :forward)


(5am:def-suite forward-suite :description "The Forward Suite")
(5am:in-suite forward-suite)

(defvar foo 4)
(defvar test-env (make-env))

(defun runt (cmds)
  (env-stack (runte cmds)))
(defmacro runte (cmds)
  `(progn
     (setf test-env (new-env))
     (run-str ,cmds test-env)
     test-env))
(defun r (str env)
  (run-str str env)
  (env-stack env))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defvar *all-test* nil))
(defmacro deftest (name &body tests)
  (let (res)
    (pushnew name *all-test*)
    (dolist (test tests)
      (if (or (eq '5am:is (car test)) (eq 'is (car test)))
	  ;; We can still have the old way.
	  (push test res)
	  (push `(5am:is (equal ',(car test) (runt ,(car (cdr test))))) res)))
    `(5am:test ,name
	       ,@res)))

(deftest
    forward1
  ((2) "1 1 +")
  ((2 1) "1 1 1 +")
  ((3) "1 1 1 + +")
  ((8) "2 2 2 + *")
  ((8) "2 2 2 + *"": foo 2 2 + ; 2 foo *"))

(deftest
    tricky
  ;; This one test to make sure that redefining words still call the older one.
  ;; See hyperstatic environment.
  ((4 2) ": foo 2 ; : bar foo ; bar : foo 4 ; bar")
  ((1 1) ": a 1 ; : b a ; : c b ; : d c ; : e d ; a e")
  ((3) "2 skip 1 2 3")
  ((0) ": foo dup 0 = if 0 else 1 - rec then drop ; 4 foo")
  ;; We had an issue, if rec is the last word the evaluation stops.
  ((0 0) ": foo dup 0 = if 0 else 1 - rec then ; 4 foo")
  ((3) ": foo skip 1 2 3 ; 2 foo")
  ((3) ": foo 2 skip 1 2 3 ; foo")
  ((1) "1 fa ! fa @")
  ((1) ": quz qw ! ; 1 quz qw @")
  ((3) "1 a push 2 a push a pop a pop +")
  ((120) ": not if nil else t then ; : fac dup 0 = not if dup a ! * a @ 1 - rec then drop ; 1 5 fac")
  ((120) ": not if nil else t then ; : fac dup 0 = not if dup >r * r> 1 - rec then drop ; 1 5 fac")
  ((12) "12 a hash hset a hash hget")
  ((2 3 4) ": 1+ 1 + ; '1+ (1 2 3) each"))

(deftest
    ifs
  ((2) ": foo if 2 else 10 then ; t foo")
  ((10) ": foo if 2 else 10 then ; nil foo")
  ((1) ": foo if 1 else 2 then ; : bar foo ; t bar")
  ((1) ": foo if 1 else 2 then ; : bar if foo then ; t t bar")
  ((2) ": foo if 1 else 2 then ; : bar foo ; nil bar")
  ((1) ": foo if 1 else 2 then ; : bar if foo else foo then ; t t bar")
  ((2) ": foo if 1 else 2 then ; : bar if foo else foo then ; nil t bar")
  ((22) ": bar if 22 else 33 then ; : foo if bar else 10 then ; t t foo"))

(defun at ()
  (5am:run! (reverse *all-test*)))
