(in-package :forward)


(5am:def-suite forward-suite :description "The Forward Suite")
(5am:in-suite forward-suite)

(defvar foo 4)
(defun run-list (cmds)
  (dolist (c cmds)
    (run c)))

(defmacro runt (cmds)
  `(progn
     (setf *stack* nil)
     (run-list ',cmds)
     *stack*))
(5am:test forward1
  (5am:is (equal '(2) (runt (1 1 +))))
  (5am:is (equal '(2 1) (runt (1 1 1 +))))
  (5am:is (equal '(3) (runt (1 1 1 + +))))
  (5am:is (equal '(8) (runt (2 2 2 + *))))
  ;; I can't test : this way tho.
  ;; (5am:is (equal '(8) (runt (|:| foo 2 2 + |;| 2 foo *))))
  )
