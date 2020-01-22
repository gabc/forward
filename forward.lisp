;;;; forward.lisp

(in-package #:forward)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(defvar *forth-readtable* (copy-readtable))
(defvar *stdlib-path* (merge-pathnames #P"lib.fw" *default-pathname-defaults*))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun stack-push (value env)
  (push value (env-stack env)))
(defun stack-pop (env)
  (pop (env-stack env)))
(defun swap (stack)
  (let ((tmp (pop stack))
	(tmpp (pop stack)))
    (push tmp stack)
    (push tmpp stack)))
(defmacro nswap (stack env)
  (let ((sname (symb  "ENV-" (symbol-name stack))))
    `(setf (,sname ,env) (swap (,sname ,env)))))

(defun code (sym env)
  (let (res)
    (setf res (mapcar #'(lambda (w) (if (word-p w) (word-name w) w)) (word-code (find-word sym env))))
    res))

(defstruct word
  name code here core immediate)
(defstruct env
  rstack stack dictionary current-word stream
  exit state nb-skip skipp defining variables
  tick)

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
      (when (equal (symbol-name name) (symbol-name (word-name w)))
	(push w res)))
    res))

(defun find-word (name env &optional (array-index nil))
  "Returns (values name t/nil) if exists."
  (unless (or (eq (type-of name) 'symbol)
	      (eq (type-of name) 'word))
    (return-from find-word (values name nil)))
  (when (eq (type-of name) 'word)
    (setf name (word-name name)))
  (unless array-index
    (setf array-index (length (env-dictionary env))))
  (dolist (word (subseq (env-dictionary env) (- (length (env-dictionary env)) array-index)))
    ;; (log:debug (symbol-name name) (symbol-name (word-name word)))
    (when (equal (symbol-name name) (symbol-name (word-name word)))
      (return-from find-word (values word t))))
  (values name nil))

(defun forth-read (env)
  (let ((*readtable* *forth-readtable*))
    (read (env-stream env))))

(defun new-env (&optional (stream t))
  (let ((env (make-env :stream stream
		       :nb-skip 0
		       :tick 0
		       :variables (make-hash-table)
		       :state :interpret)))
    (build-dictionary env)
    (load-file *stdlib-path* env)
    env))

(defun forward ()
  (let ((env (new-env *standard-input*)))
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
     (if (eq (car word) 'QUOTE)
	 (stack-push (car (cdr word)) env)
	 (stack-push word env)))
    (symbol
     (log:debug "Is symbol ~s" word)
     (stack-push word env))
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
     (if (not (env-defining env))
	 (compile-new-word word env)
	 (push word (word-code (env-defining env)))))
    (cons
     (if (eq (car word) 'QUOTE)
	 (push (car (cdr  word)) (word-code (env-defining env)))
	 (push word (word-code (env-defining env)))))
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
    (let (name)
      (if (eq (type-of word) 'word)
	  (setf name (word-name word))
	  (setf name word))
      (setf (env-defining env) (make-word :name name :core nil
					  :here (length (env-dictionary env)))))))

(defun load-file (path env)
  (with-open-file (fd path)
    (loop for line = (read-line fd nil nil)
       while line
       do (run-str line env))))

(defun run-str (str env)
  (with-open-stream (s (make-string-input-stream str))
    (setf (env-stream env) s)
    (let (words)
      (handler-case
	  (loop while (not (env-exit env))
	     do
	       (let ((word (forth-read env)))
		 (push word words)))
	(end-of-file (c)
	  (declare (ignore c))))
      (run (reverse words) env))))

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
  (tagbody
   recurse
     (dolist (w word)
       (incf (env-tick env))
       (log:debug "Running here ~s ~s" w (env-stack env))
       (if (> (env-nb-skip env) 0)
	   (progn
	     (log:debug "skipping ~s" w)
	     (decf (env-nb-skip env)))
	   (multiple-value-bind (entry exist) (find-word w env)
	     (declare (ignore exist))
	     (log:debug (env-state env))
	     (run-word entry env)
	     (when (eq :recurse (car (env-rstack env)))
	       (log:debug "recured")
	       (pop (env-rstack env))	;pop :recurse
	       (go recurse)))))))

(defun skip (nb env)
  (setf (env-skipp env) t)
  (setf (env-nb-skip env) nb))

(defun clean-name (symbol)
  (case symbol
    (|:| 'colon)
    (|;| 'semi-col)
    (otherwise symbol)))

(defmacro define-word (name core immediate &body body)
  (let* ((real-name (clean-name name))
	 (fn-name (symb "FW-" real-name)))
    `(progn
       (defun ,fn-name (env) ,@body)
       (setf *base-dictionary*
	     (remove-if #'(lambda (el) (eq ',name (car el))) *base-dictionary*))
       (push '(,name ,core ,immediate ,fn-name)
	     *base-dictionary*))))




(set-macro-character #\: (lambda (stream char) '|:|) t *forth-readtable*)
(set-macro-character #\; (lambda (stream char) '|;|) t *forth-readtable*)

;;; Words.
(defvar *base-dictionary* nil)

(defun build-dictionary (env)
  "Builds new dictionary for the env, ENV"
  (setf (env-dictionary env) nil)
  (dolist (new-word *base-dictionary*)
    (multiple-value-bind (name core immediate body) (values-list new-word)
      (push (make-word :name name
		       :code body
		       :here (length (env-dictionary env))
		       :core core
		       :immediate immediate)
	    (env-dictionary env)))))

(define-word s  t nil
  (format t "~s" (reverse (env-stack env))))

(define-word rs  t nil
  (format t "~s" (reverse (env-rstack env))))

(define-word >r  t nil
  (progn
    (push (stack-pop env) (env-rstack env))
    (setf (env-rstack env) (swap (env-rstack env)))))

(define-word r>  t nil
  (progn
    (setf (env-rstack env) (swap (env-rstack env)))
    (push (pop (env-rstack env)) (env-stack env))))

(define-word drop t nil
  (pop (env-stack env)))

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
  (print (code (stack-pop env) env)))
(define-word _  t nil
  (print (stack-pop env)))
(define-word call t nil
  (let ((fn (stack-pop env))
	(arg (stack-pop env)))
    (log:debug "Fn calling ~s with ~s, res: ~s" fn arg (funcall fn arg))
    (stack-push (funcall fn arg) env)))
(define-word each t nil
  (let ((list (stack-pop env))
	(word (stack-pop env)))
    (log:debug "Each-pre ~S ~S" list word)
    (dolist (l (reverse list))
      (stack-push l env)
      (log:debug "Eaching: ~s ~s" l (env-stack env))
      (run (list word) env))))
(define-word eql t nil
  (stack-push (equal (stack-pop env) (stack-pop env)) env))
(define-word =  t nil
  (stack-push (= (stack-pop env) (stack-pop env)) env))
(define-word clear  t nil
  (setf (env-stack env) nil))

(define-word immediate  t nil
  (setf (word-immediate (car (env-dictionary env))) t))

(define-word if t t
  (progn
    (assemble 0 env)
    (stack-push (word-code (env-defining env)) env)
    (stack-push (env-tick env) env)
    (assemble (find-word '?skip env) env)))

(define-word else  t t
  (let* ((old-tick (stack-pop env))
	 (word (stack-pop env)))
    (assemble 0 env)
    (stack-push (word-code (env-defining env)) env)
    (stack-push (env-tick env) env)
    (assemble (find-word 'skip env) env)
    (log:debug "else ticks o:~s n:~s" old-tick (env-tick env))
    (rplaca word (+ 1 (- (env-tick env) old-tick)))))

(define-word then t t
  (let* ((old-tick (stack-pop env))
	 (word (stack-pop env)))
    (log:debug "then ticks o:~s n:~s" old-tick (env-tick env))
    ;; Remove 1 because then will not appear in the word.
    (rplaca word (- (env-tick env) old-tick 1))))

(define-word hget t nil
  ;; key my-hash hget
  (let ((place (stack-pop env))
	(key (stack-pop env)))
    (stack-push (gethash key (gethash place (env-variables env))) env)))
(define-word hset t nil
  ;; val key my-hash hset
  (let ((place (stack-pop env))
	(key (stack-pop env))
	(val (stack-pop env)))
    (unless (gethash place (env-variables env))
      (setf (gethash place (env-variables env)) (make-hash-table)))
    (setf (gethash key (gethash place (env-variables env))) val)))
(define-word push t nil
  (let ((place (stack-pop env))
	(val (stack-pop env)))
    (push val
	  (gethash place (env-variables env)))))
(define-word pop t nil
  (stack-push
   (pop
    (gethash (stack-pop env) (env-variables env)))
   env))
(define-word ?skip t nil
  (progn
    (nswap stack env)
    (let ((test (stack-pop env))
	  (amount (stack-pop env)))
     (if (eq nil test)
	 (skip amount env)))))
(define-word skip  t nil
  (skip (stack-pop env) env))

(define-word rec  t nil
  (progn
    (log:debug "recurseeed")
    (push :recurse (env-rstack env))
    (nswap rstack env)))

(define-word |:|  t nil
    (setf (env-state env) :compile))

(define-word |;|  t t
  (setf (env-state env) :interpret)
  (add-word (word-name (env-defining env))
	    (nreverse (word-code (env-defining env)))env)
  (setf (env-defining env) nil))

(define-word |!| t nil
  (let ((var (stack-pop env))
	(value (stack-pop env)))
    (setf (gethash var (env-variables env)) value)))

(define-word |@| t nil
  (let ((var (stack-pop env)))
    (stack-push (gethash var (env-variables env)) env)))
(define-word and t nil
  (stack-push (and (stack-pop env)
		   (stack-pop env))
   env))
(define-word d t nil
  (log:debug "In ~s: stack: ~s" (word-name (second (env-rstack env))) (env-stack env)))
