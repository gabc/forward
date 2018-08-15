;;;; forward.lisp

(in-package #:forward)

(defvar *dictionary* '()
  "actually a list of words.")
(defvar *stack* '())

(defun stack-push (value)
  (push value *stack*))
(defun make-word (name code &optional (core nil))
  (let ((new-word name))
    (setf (get new-word 'name) name)
    (setf (get new-word 'code) code)
    (setf (get new-word 'here) (length *dictionary*))
    (setf (get new-word 'core) core) ; Does it get `eval'd or recursively `eval'd?
    (push new-word *dictionary*)))

(defun find-word (name &optional (array-index nil array-index-p))
  (flet ((find-word-from-end (name)
           (dolist (word *dictionary*)
             (when (eq name (car word))
               word)))
         (find-word-from (name array-index) ; Support redifinition ?
           (values name array-index))
         )
   (if array-index-p
       (find-word-from-end name)
       (find-word-from name array-index))))

(defun forward ()
  (loop
     (let ((word (read #| Maybe this instead? (read *standard-input* t #\Space)|#)))
       (if (eq word (intern "Q")) ; why can't I just do (eq word 'q) ?!
           (return-from forward)
           (format t "Value ~A: type ~a~%" word (type-of word))))))

(defun run (entry)
  (when (get entry 'core)               ; Just run the code
    (stack-push (eval (get entry 'code)))
    (return-from run))
  (let ((code (get entry 'code)))
    (dolist (word code)
      (when word
        ;; Don't push the value on the stack since
        ;; it would do it twice whenever it hits a 'core entry
        (run word)))))
