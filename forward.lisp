;;;; forward.lisp

(in-package #:forward)

(defvar *dictionary* '()
  "actually a list of words.")
(defvar *forth-readtable* (copy-readtable))
(defvar *stack* '())
(defvar *exit-flag* nil
  "Can't think of a better way to leave the thing.")

(defun stack-push (value)
  (push value *stack*))
(defun stack-pop ()
  (pop *stack*))
(defstruct word
  name code here core)
(defun add-word (name code &optional (core nil))
  (let ((new-word (make-word :name name
			     :code (if core
				       (eval `(lambda (s) (declare (ignore s)) ,code))
				       code)
			     :here (length *dictionary*)
			     :core core))) 
    (push new-word *dictionary*)))

(defun drop-word (name)
  (delete (find-word name) *dictionary*))

(defun find-word (name &optional (array-index nil array-index-p))
  "Returns (values name t/nil) if exists."
  (flet ((find-word-from-end (name)
           (dolist (word *dictionary*)
	     ;; (log:debug word)
             (when (eq name (word-name word))
               (return-from find-word-from-end (values word t))))
	   (values name nil))
         (find-word-from (name array-index)
	   ;; Support redifinition but statically. Not all the word.
           (values name array-index)))
    (if array-index-p
	(find-word-from name array-index)
	(find-word-from-end name))))

(defun forth-read (&optional stream)
  (let ((*readtable* *forth-readtable*))
    (if stream
	(read stream)
	(read))))

(defun forward ()
  (setf *exit-flag* nil)
  (loop while (not *exit-flag*)
       do
       (let ((word (forth-read #| Maybe this instead? (read *standard-input* t #\Space)|#)))
         (run word *standard-input*))))

(defun run (word stream)
  (flet ((run-word (entry)
           (when (word-core entry)      ; Just run the code
             (funcall (word-code entry) stream)
             (return-from run))
           (let ((code (word-code entry)))
             (dolist (word code)
	       (log:debug word)
	       (let ((w (find-word word (word-here entry))))
		 (run w stream))))))
    (when word
      (etypecase word
        (cons
         (when (eq (car word) 'QUOTE)
	   (stack-push (car (cdr word)))))
        (simple-array
         (stack-push word))
        (number
         (stack-push word))
        (symbol
	 (log:debug "Is symbol ~s" word)
	 (multiple-value-bind (w exist) (find-word word)
	   (if exist
	       (run-word w)
	       (progn (log:debug "No exists ~s" w)
		      (when (boundp word)
			(log:debug "Evaled ~s" (eval word))
			(stack-push (eval word)))))))))))
(defun init-dict ()
  (add-word 's '(format t "~s" *stack*) t)
  (add-word '+ '(stack-push (+ (stack-pop) (stack-pop))) t)
  (add-word '* '(stack-push (* (stack-pop) (stack-pop))) t)
  (add-word '- '(let ((temp (stack-pop))) (stack-push (- (stack-pop) temp))) t)
  (add-word 'q '(setf *exit-flag* t) t)
  (add-word 'code '(print (symbol-plist (find-word (intern (string-upcase (stack-pop)))))) t)
  (add-word '_ '(print (stack-pop)) t)
  (add-word '= '(stack-push (= (stack-pop) (stack-pop)) t))
  (add-word 'clear '(setf *stack* nil) t)
					;(add-word 'if)
  (add-word '|:| '(let (code temp name)
		    (setf name (forth-read s))
		    (loop while (not (eq (setf temp (forth-read s)) '|;|))
		       do (push temp code))
		    (add-word name (nreverse code)))
	     t))
(setf *dictionary* nil)
(init-dict)

(set-macro-character #\: (lambda (stream char) '|:|) t *forth-readtable*)
(set-macro-character #\; (lambda (stream char) '|;|) t *forth-readtable*)

