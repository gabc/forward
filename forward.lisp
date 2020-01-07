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
(defun make-word (name code &optional (core nil))
  (let ((new-word name))
    (setf (get new-word 'name) name)
    (setf (get new-word 'code) (if core (eval `(lambda () ,code)) code))
    (setf (get new-word 'here) (length *dictionary*))
    (setf (get new-word 'core) core) ; Does it get `eval'd or recursively `eval'd?
    (push new-word *dictionary*)))

(defun drop-word (name)
  (delete (find-word name) *dictionary*))

(defun find-word (name &optional (array-index nil array-index-p))
  "Returns (values name t/nil) if exists."
  (flet ((find-word-from-end (name)
           (dolist (word *dictionary*)
	     ;; (log:debug word)
             (when (eq name (get word 'name))
               (return-from find-word-from-end (values word t))))
	   (values name nil))
         (find-word-from (name array-index)
	   ;; Support redifinition but statically. Not all the word.
           (values name array-index)))
    (if array-index-p
	(find-word-from name array-index)
	(find-word-from-end name))))

(defun forth-read ()
  (let ((*readtable* *forth-readtable*))
    (read)))

(defun forward ()
  (setf *exit-flag* nil)
  (loop while (not *exit-flag*)
       do
       (let ((word (forth-read #| Maybe this instead? (read *standard-input* t #\Space)|#)))
         (run word))))

(defun run (word)
  (flet ((run-word (entry)
           (when (get entry 'core)      ; Just run the code
             (funcall (get entry 'code))
             (return-from run))
           (let ((code (get entry 'code)))
             (dolist (word code)
               (run word)))))
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

(make-word 's '(format t "~s" *stack*) t)
(make-word '+ '(stack-push (+ (stack-pop) (stack-pop))) t)
(make-word '* '(stack-push (* (stack-pop) (stack-pop))) t)
(make-word '- '(let ((temp (stack-pop))) (stack-push (- (stack-pop) temp))) t)
(make-word 'q '(setf *exit-flag* t) t)
(make-word 'code '(print (symbol-plist (find-word (intern (string-upcase (stack-pop)))))) t)
(make-word '_ '(print (stack-pop)) t)
(make-word '= '(stack-push (= (stack-pop) (stack-pop)) t))
(make-word 'clear '(setf *stack* nil) t)
					;(make-word 'if)
(make-word '|:| '(let (code temp name)
		  (setf name (forth-read))
		  (loop while (not (eq (setf temp (forth-read)) '|;|))
		     do (push temp code))
		  (make-word name (nreverse code)))
	   t)

(set-macro-character #\: (lambda (stream char) '|:|) t *forth-readtable*)
(set-macro-character #\; (lambda (stream char) '|;|) t *forth-readtable*)

