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
  rstack stack dictionary current-word stream exit state nb-skip skipp)

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

(defmacro dowords ((name env) &body body)
  `(dolist (,name (env-dictionary ,env))
     ,@body))

(defun find-all-words (name env)
  (let (res)
    (dowords (w env)
      (when (equal name (word-name w))
	(push w res)))
    res))

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

(defmacro with-rstack (word env &body body)
  `(progn
     (push ,word (env-rstack ,env))
     ,@body
     (pop (env-rstack ,env))))

(defun interpret  (word env)
  (log:debug word)
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
     (if (word-core word)		; Just run the code
	 (funcall (word-code word) env)
	 (run (word-code word) env)))))

(defun assemble (word env)
  (etypecase word
    (simple-array
     (push word (word-code (env-current-word env))))
    (symbol
     (compile-new-word word env))
    (cons
     (when (eq (car word) 'QUOTE)
       (push (car (cdr  word)) (word-code (env-current-word env)))))
    (number
     (push word (word-code (env-current-word env))))
    (word
     (if (not (env-current-word env))
	 (compile-new-word word env)
	 (if (word-immediate word)
	     (progn
	       (log:debug "immediate, compile ~s" word)
	       (interpret word env))
	     (progn
	       (log:debug "nonimmediate, compile ~s" word)
	       (push word (word-code (env-current-word env)))))))))
(defun compile-new-word (word env)
  (when (not (env-current-word env))
    (log:debug word)
    (setf (env-current-word env) (make-word :name word :core nil
					    :here (length (env-dictionary env))))))

(defun run-word (word env)
  (log:debug word)
  (case (env-state env)
    (:interpret
     (interpret word env))
    (:compile
     (log:debug "compiling ~s" word)
     (log:debug (when (env-current-word env) (word-code (env-current-word env))))
     (assemble word env))))

(defun run (word env)
  (declare (optimize (speed 0) (space 0) (debug 3)))
  (dolist (w word)
    (if (> (env-nb-skip env) 0)
	(progn
	  (log:debug "skipping ~s" w)
	  (decf (env-nb-skip env)))
	(multiple-value-bind (entry exist) (find-word w env)
	  (declare (ignore exist))
	  (log:debug (env-state env))
	  (with-rstack entry env
	    (run-word entry env))))))

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
  (add-word 'if '(let (then-branch else-branch code stt)
		  (setf code (word-code (car (last (env-rstack env)))))
		  (log:debug code)
		  (dolist (w code)
		    (log:debug w)
		    (case (and (word-p w) (setf w (word-name w)))
		      (if (setf stt :if))
		      (else (setf stt :else))
		      (then (setf stt :then)))
		    (unless (member w '(if then else))
		      (case stt
			(:if (push w then-branch))
			(:else (push w else-branch)))))
		  (setf then-branch (reverse then-branch)) 
		  (setf else-branch (reverse else-branch))
		  ;; (pop then-branch) (pop else-branch)
		  (log:debug then-branch)
		  (log:debug else-branch)
		  (if (eq t (stack-pop env))
		      (progn
			(run-list then-branch env)
			)
		      (run-list else-branch env)))
	    env t)
  (add-word 'else '(declare (ignore env)) env t)
  (add-word 'then '(declare (ignore env)) env t)
  (add-word 'skip '(progn (setf (env-skipp env) t)
		    (setf (env-nb-skip env) (stack-pop env)))
	    env t)
  (add-word '|:| '(setf (env-state env) :compile) env t)
  (add-word '|;| '(progn
		   (setf (env-state env) :interpret)
		   (add-word (word-name (env-current-word env))
		    (nreverse (word-code (env-current-word env)))env)
		   (setf (env-current-word env) nil))
	    env t t))

(set-macro-character #\: (lambda (stream char) '|:|) t *forth-readtable*)
(set-macro-character #\; (lambda (stream char) '|;|) t *forth-readtable*)

