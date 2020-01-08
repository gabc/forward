(in-package :forward)


(5am:def-suite forward-suite :description "The Forward Suite")
(5am:in-suite forward-suite)

(defvar foo 4)
(defun run-str (str)
  (with-open-stream (s (make-string-input-stream str))
    (setf *exit-flag* nil)
    (handler-case
	(loop while (not *exit-flag*)
	   do
	     (let ((word (forth-read s)))
               (run word s)))
      (end-of-file (c)
	(declare (ignore c))))))

(defmacro runt (cmds)
  `(progn
     (setf *stack* nil)
      (run-str ,cmds)
     *stack*))

(5am:test
 forward1
 (5am:is (equal '(2) (runt "1 1 +")))
 (5am:is (equal '(2 1) (runt "1 1 1 +")))
 (5am:is (equal '(3) (runt "1 1 1 + +")))
 (5am:is (equal '(8) (runt "2 2 2 + *")))
 (5am:is (equal '(8) (runt ": foo 2 2 + ; 2 foo *"))))
