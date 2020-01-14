;;;; forward.lisp

(in-package #:forward)

(defvar *dictionary* '()
  "actually a list of words.")
(defvar *forth-readtable* (copy-readtable))
(defvar *stack* '())
(defvar *exit-flag* nil
  "Can't think of a better way to leave the thing.")

(defun stack-push (value env)
  (push value (env-stack env)))
(defun stack-pop (env)
  (pop (env-stack env)))

(defstruct word
  name code here core immediate)
(defstruct env
  rstack stack dictionary current-word stream exit state)

(defun add-word (name code env &optional (core nil) immediate)
  (let ((new-word (make-word :name name
			     :code (if core
				       (eval `(lambda (env) ,code))
				       code)
			     :here (length (env-dictionary env))
			     :core core
			     :immediate immediate))) 
    (push new-word (env-dictionary env))))

(defun drop-word (name env)
  (delete (find-word name env) (env-dictionary env)))

(defun find-word (name env &optional (array-index nil))
  "Returns (values name t/nil) if exists."
  (unless array-index
    (setf array-index (length (env-dictionary env))))
  (dolist (word (subseq (env-dictionary env) (- (length (env-dictionary env)) array-index)))
    ;; (log:debug word)
    (when (equal name (word-name word))
      (return-from find-word (values word t))))
  (values name nil))

(defun forth-read (env)
  (let ((*readtable* *forth-readtable*))
    (read (env-stream env))))

(defun forward ()
  (let ((env (make-env)))
    (setf (env-stream env) *standard-input*)
    (init-dict env)
    (loop while (not (env-exit env))
       do
	 (let ((word (forth-read env)))
           (run word env)))
    env))

(defun run-list (list env)
  (dolist (w list)
    (run (find-word w env) env)))

(defmacro with-rstack (word env &body body)
  `(progn
     (push ,word (env-rstack ,env))
     ,@body
     (pop (env-rstack ,env))))

(defun run (word env)
  (labels ((interpret (word env)
	     (log:debug word)
	     (let ((code (word-code word)))
	       (if (word-core word)
		   (funcall code env)
		   (dolist (word code)
		     (etypecase word
		       (simple-array
			(stack-push word env))
		       (number
			(stack-push word env))
		       (cons
			(when (eq (car word) 'QUOTE)
			  (stack-push (car (cdr word)) env)))
		       (symbol
			(log:debug "Is symbol ~s" word)
			(when (boundp word)
			  (log:debug "Evaled ~s" (eval word))
			  (stack-push (eval word) env)))
		       (word
			(if (word-core word) ; Just run the code
			    (funcall (word-code word) env)
			    (let ((w (find-word word env (word-here word))))
			      (run w env))))))))) 
	   (run-word (word env)
	     (log:debug word)
	     (case (env-state env)
	       (:interpret
		(log:debug "interpreting")
		(interpret word env))
	       (:compile
		(log:debug "compiling ~s" word)
		(log:debug (when (env-current-word env) (word-code (env-current-word env))))
		(etypecase word
		  (simple-array
		   (push word (word-code (env-current-word env))))
		  (symbol
		   (when (not (env-current-word env))
		     (log:debug "getting here?")
		     (setf (env-current-word env) (make-word :name word :core nil
							     :here (length (env-dictionary env))))))
		  (cons
		   (when (eq (car word) 'QUOTE)
		     (push (car (cdr  word)) (word-code (env-current-word env)))))
		  (number
		   (push word (word-code (env-current-word env))))
		  (word
		   (if (word-immediate word)
		       (progn
			 (log:debug "wat")
			 (interpret word env))
		       (progn
			 (log:debug "wherer")
			 (push word (word-code (env-current-word env)))))))))))
    (multiple-value-bind (entry exist) (find-word word env)
      (declare (ignore exist))
      (log:debug (env-state env))
      (with-rstack entry env
	(run-word entry env)))))

(defun init-dict (env)
  (add-word 's '(format t "~s" (reverse (env-stack env))) env t)
  (add-word 'rs '(format t "~s" (reverse (env-rstack env))) env t)
  ;; (add-word '>r '(push (stack-pop env) (env-rstack env)) env t)
  ;; (add-word 'r> '(push (pop (env-rstack env)) (env-stack env)) env t)
  (add-word '+ '(stack-push (+ (stack-pop env) (stack-pop env)) env) env t)
  (add-word '* '(stack-push (* (stack-pop env) (stack-pop env)) env) env t)
  (add-word '- '(let ((temp (stack-pop env))) (stack-push (- (stack-pop env) temp) env)) env t)
  (add-word 'q '(setf (env-exit env) t) env t)
  (add-word 'code '(print (word-code (find-word (stack-pop env) env))) env t)
  (add-word '_ '(print (stack-pop env)) env t)
  (add-word '= '(stack-push (= (stack-pop env) (stack-pop env)) env) env t)
  (add-word 'clear '(setf (env-stack env) nil) env t)
  (add-word 'immediate '(setf (word-immediate (car (env-dictionary env))) t) env t)
  (add-word '\(comment '(let (tmp)
			 (log:debug (env-stream env))
			 (loop while (not (eq (setf tmp (forth-read env)) '\))))) env t)
  ;; (add-word 'if '(let (then-branch else-branch code stt)
  ;; 		  (setf code (word-code (car (last (env-rstack env)))))
  ;; 		  (log:debug code)
  ;; 		  (dolist (w code) 
  ;; 		    (case w
  ;; 		      (if (setf stt :if))
  ;; 		      (else (setf stt :else))
  ;; 		      (then (setf stt :then)))
  ;; 		    (case stt
  ;; 		      (:if (push w then-branch))
  ;; 		      (:else (push w else-branch))))
  ;; 		  (nreverse then-branch)
  ;; 		  (nreverse else-branch)
  ;; 		  ;; (pop then-branch) (pop else-branch)
  ;; 		  (log:debug then-branch)
  ;; 		  (log:debug else-branch)
  ;; 		  (if (stack-pop env)
  ;; 		      (run-list then-branch env)
  ;; 		      (run-list else-branch env)))
  ;; 	    env t)
  (add-word '|:| '(setf (env-state env) :compile) env t)
  (add-word '|;| '(progn
		   (setf (env-state env) :interpret)
		   (add-word (word-name (env-current-word env))
		    (nreverse (word-code (env-current-word env)))env)
		   (setf (env-current-word env) nil))
	    env t t))

(set-macro-character #\: (lambda (stream char) '|:|) t *forth-readtable*)
(set-macro-character #\; (lambda (stream char) '|;|) t *forth-readtable*)

