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

(defvar *all-test* nil)
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
  ((0 0) ": foo dup 0 = if 0 else 1 - rec then ; 4 foo")
  ((3) ": foo skip 1 2 3 ; 2 foo")
  ((3) ": foo 2 skip 1 2 3 ; foo")
  ((1) "1 fa ! fa @")
  ((1) ": quz qw ! ; 1 quz qw @")
  ((120) ": not if nil else t then ; : fac dup 0 = not if dup a ! * a @ 1 - rec then drop ; 1 5 fac")
  ((120) ": not if nil else t then ; : fac dup 0 = not if dup >r * r> 1 - rec then drop ; 1 5 fac"))

(deftest
 ifs
 ((2) ": foo if 2 else 10 then ; t foo")
 ((10) ": foo if 2 else 10 then ; nil foo")
 ((1) ": foo if 1 else 2 then ; : bar foo ; t bar")
 ((2) ": foo if 1 else 2 then ; : bar foo ; nil bar")
 ((22) ": bar if 22 else 33 then ; : foo if bar else 10 then ; t t foo"))

(defun at ()
  (5am:run! (reverse *all-test*)))
