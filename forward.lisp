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
(defun swap (stack)
  (let ((tmp (pop stack))
	(tmpp (pop stack)))
    (push tmp stack)
    (push tmpp stack)))

(defstruct word
  name code here core immediate)
(defstruct env
  rstack stack dictionary current-word stream exit state nb-skip skipp defining)

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

(defun clean-all (env)
  (setf (env-stream env) t)
  (setf (env-dictionary env) nil)
  (setf (env-stack env) nil)
  (setf (env-skipp env) nil)
  (setf (env-nb-skip env) 0)
  (setf (env-state env) :interpret)
  (build-dictionary env))

(defun forward ()
  (let ((env (make-env)))
    (clean-all env)
    (setf (env-stream env) *standard-input*)
    (build-dictionary env)
    (loop while (not (env-exit env))
       do
	 (let ((word (forth-read env)))
           (run (list word) env)))
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
	 (progn      (setf (env-current-word env) word)
	  (run (word-code word) env))))))

(defun assemble (word env)
  (etypecase word
    (simple-array
     (push word (word-code (env-defining env))))
    (symbol
     (compile-new-word word env))
    (cons
     (when (eq (car word) 'QUOTE)
       (push (car (cdr  word)) (word-code (env-defining env)))))
    (number
     (push word (word-code (env-defining env))))
    (word
     (if (not (env-defining env))
	 (compile-new-word word env)
	 (if (word-immediate word)
	     (progn
	       (log:debug "immediate, compile ~s" word)
	       (with-rstack word env
		(interpret word env)))
	     (progn
	       (log:debug "nonimmediate, compile ~s" word)
	       (push word (word-code (env-defining env)))))))))

(defun compile-new-word (word env)
  (when (not (env-defining env))
    (log:debug word)
    (setf (env-defining env) (make-word :name word :core nil
					:here (length (env-dictionary env))))))

(defun run-word (word env)
  (log:debug word)
  (case (env-state env)
    (:interpret
     (with-rstack word env
      (interpret word env)))
    (:compile
     (log:debug "compiling ~s" word)
     (log:debug (when (env-defining env) (word-code (env-defining env))))
     (assemble word env))))

(defun run (word env)
  (declare (optimize (speed 0) (space 0) (debug 3)))
  (tagbody
     recurse
     (dolist (w word)
       (log:debug (env-rstack env))
       (if (> (env-nb-skip env) 0)
	   (progn
	     (log:debug "skipping ~s" w)
	     (decf (env-nb-skip env)))
	   (multiple-value-bind (entry exist) (find-word w env)
	     (declare (ignore exist))
	     (when (eq :recurse (car (env-rstack env)))
	       (log:debug "recured")
	       (pop (env-rstack env))
	       (go recurse))
	     (log:debug (env-state env))
	     (run-word entry env))))))

(defmacro define-word (name core immediate body)
  `(progn
     (setf *base-dictionary*
	   (remove-if #'(lambda (el) (eq ',name (car el))) *base-dictionary*))
     (push '(,name ,core ,immediate ,body)
	   *base-dictionary*)))


(defun build-dictionary (env)
  "Builds new dictionary for the env, ENV"
  (setf (env-dictionary env) nil)
  (dolist (new-word *base-dictionary*)
    (multiple-value-bind (name core immediate body) (values-list new-word)
      (push (make-word :name name
		       :code (if core
				 (eval `(lambda (env) ,body))
				 body)
		       :here (length (env-dictionary env))
		       :core core
		       :immediate immediate)
	    (env-dictionary env)))))

(set-macro-character #\: (lambda (stream char) '|:|) t *forth-readtable*)
(set-macro-character #\; (lambda (stream char) '|;|) t *forth-readtable*)

;;; Words.
(defvar *base-dictionary* nil)

(define-word s  t nil
  (format t "~s" (reverse (env-stack env))))

(define-word rs  t nil
  (format t "~s" (reverse (env-rstack env))))

(define-word >r  t nil
  (push (stack-pop env) (env-rstack env)))

(define-word r>  t nil
  (push (pop (env-rstack env)) (env-stack env)))

(define-word dup  t nil
  (let ((tmp (stack-pop env)))
    (push tmp (env-stack env))
    (push tmp (env-stack env))))

(define-word swap  t nil
  (setf (env-stack env) (swap (env-stack env))))

(define-word +  t nil
  (stack-push (+ (stack-pop env) (stack-pop env)) env))
(define-word *  t nil
    (stack-push (* (stack-pop env) (stack-pop env)) env))

(define-word -  t nil
  (let ((temp (stack-pop env))) (stack-push (- (stack-pop env) temp) env)))

(define-word q  t nil
  (setf (env-exit env) t))
(define-word code  t nil
  (let (res)
    (setf res (mapcar #'(lambda (w) (if (word-p w) (word-name w) w)) (word-code (find-word (stack-pop env) env))))
    (print res)))
(define-word _  t nil
  (print (stack-pop env)))
(define-word =  t nil
    (stack-push (= (stack-pop env) (stack-pop env)) env))
(define-word clear  t nil
  (setf (env-stack env) nil))

(define-word immediate  t nil
  (setf (word-immediate (car (env-dictionary env))) t))

(define-word if  t nil
  (let (branch code stt)
    (setf code (word-code (second (env-rstack env))))
    (log:debug code)
    (dolist (w code)
      (when (not (eq stt :done))
	(log:debug w)
	(case (and (word-p w) (setf w (word-name w)))
	  (if (setf stt :if))
	  ((else then) (setf stt :done)))
	(unless (member w '(if then else))
	  (case stt
	    ((:done :if) (push w branch))))))
    (log:debug "if branch ~s" branch)
    (log:debug (length branch))
    (if (eq t (stack-pop env))
	(push :did-if (env-rstack env))
	(progn (push :did-not-if (env-rstack env))
	       (setf (env-nb-skip env) (length branch)
		     (env-skipp env) t))) 
    (setf (env-rstack env) (swap (env-rstack env)))))

(define-word else  t nil
  (let (branch code stt tmp)
    (setf (env-rstack env) (swap (env-rstack env)))
    (setf tmp (pop (env-rstack env)))
    (setf code (word-code (second (env-rstack env))))
    (log:debug code)
    (dolist (w code)
      (when (not (eq stt :done))
	(log:debug w)
	(case (and (word-p w) (setf w (word-name w)))
	  (else (setf stt :if))
	  (then (setf stt :done)))
	(unless (member w '(if then else))
	  (case stt
	    ((:done :if) (push w branch))))))
    (log:debug "else branch ~s" branch)
    (log:debug (length branch))
		    
    (case tmp
      (:did-if
       (setf (env-nb-skip env) (length branch))
       (setf (env-skipp env) t))
      (:did-not-if (setf (env-nb-skip env) 0)
		   (setf (env-skipp env) nil))
      (t (push tmp (env-rstack env))))))

(define-word then  t nil
  (log:debug "then ~s" (env-rstack env)))

(define-word skip  t nil
  (progn
   (setf (env-skipp env) t)
   (setf (env-nb-skip env) (stack-pop env))))

(define-word rec  t nil
  (progn (push :recurse (env-rstack env))
	 (setf (env-rstack env) (swap (env-rstack env)))))

(define-word |:|  t nil
    (setf (env-state env) :compile))

(define-word |;|  t t
    (progn
      (setf (env-state env) :interpret)
      (add-word (word-name (env-defining env))
		(nreverse (word-code (env-defining env)))env)
      (setf (env-defining env) nil)))



