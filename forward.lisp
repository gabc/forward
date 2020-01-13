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
  name code here core)
(defstruct env
  stack dictionary current-word stream exit)

(defun add-word (name code env &optional (core nil))
  (let ((new-word (make-word :name name
			     :code (if core
				       (eval `(lambda (env) ,code))
				       code)
			     :here (length (env-dictionary env))
			     :core core))) 
    (push new-word (env-dictionary env))))

(defun drop-word (name env)
  (delete (find-word name env) (env-dictionary env)))

(defun find-word (name env &optional (array-index nil))
  "Returns (values name t/nil) if exists."
  (unless array-index
    (setf array-index (length (env-dictionary env))))
  (dolist (word (subseq (env-dictionary env) (- (length (env-dictionary env)) array-index)))
    (log:debug word)
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
           (run word env)))))

(defun run-list (list env)
  (dolist (w list)
    (run (find-word w env) env)))

(defun run (word env)
  (flet ((run-word (entry)
           (when (word-core entry)      ; Just run the code
             (funcall (word-code entry) env)
             (return-from run))
           (let ((code (word-code entry)))
             (dolist (word code)
	       (log:debug word)
	       (let ((w (find-word word env (word-here entry))))
		 (run w env))))))
    (multiple-value-bind (entry exist) (find-word word env)
      (declare (ignore exist))
      (etypecase entry
	(cons
	 (when (eq (car entry) 'QUOTE)
	   (stack-push (car (cdr entry)) env)))
	(simple-array
	 (stack-push entry env))
	(number
	 (stack-push entry env))
	(symbol
	 (log:debug "Is symbol ~s" entry)
	 (when (boundp entry)
	   (log:debug "Evaled ~s" (eval entry))
	   (stack-push (eval entry) env)))
	(word
	 (log:debug entry)
	 (run-word entry))))))

(defun init-dict (env)
  (add-word 's '(format t "~s" (reverse (env-stack env))) env t)
  (add-word '+ '(stack-push (+ (stack-pop env) (stack-pop env)) env) env t)
  (add-word '* '(stack-push (* (stack-pop env) (stack-pop env)) env) env t)
  (add-word '- '(let ((temp (stack-pop env))) (stack-push (- (stack-pop env) temp) env)) env t)
  (add-word 'q '(setf (env-exit env) t) env t)
  (add-word 'code '(print (word-code (find-word (stack-pop env) env))) env t)
  (add-word '_ '(print (stack-pop env)) env t)
  (add-word '= '(stack-push (= (stack-pop env) (stack-pop env)) env) env t)
  (add-word 'clear '(setf (env-stack env) nil) env t)
  (add-word '\(comment '(let (tmp)
			 (loop while (not (eq (setf tmp (forth-read env)) '\))))) env t)
  (add-word 'if '(let (then-branch else-branch temp)
		  (loop while (not (member (setf temp (forth-read env)) '(else then)))
		     do (push temp then-branch))
		  (unless (eq temp 'then) ;If we just ate the end don't eat more.
		    (loop while (not (eq (setf temp (forth-read env)) 'then))
		       do (push temp else-branch)))
		  (log:debug then-branch)
		  (log:debug else-branch)
		  (if (stack-pop env)
		      (run-list then-branch env)
		      (run-list else-branch env)))
	    env t)
  (add-word '|:| '(let (code temp name)
                   (setf name (forth-read env))
                   (loop while (not (eq (setf temp (forth-read env)) '|;|))
		      do (push temp code))
                   (add-word name (nreverse code) env))
	    env t))

(set-macro-character #\: (lambda (stream char) '|:|) t *forth-readtable*)
(set-macro-character #\; (lambda (stream char) '|;|) t *forth-readtable*)

