(in-package :forward)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))
  (defun clean-name (symbol)
    (case symbol
      (|:| 'colon)
      (|;| 'semi-col)
      (otherwise symbol)))
  (defun extract-rank (list)
    (flet ((iscons (x) (eq (type-of x) 'CONS)))
      (if (and (iscons list)
               (mapcar #'iscons list))
          (length list)
          0)))
  (defun ensure-same-lenght (lists)
    (let (res)
      (when (> (length lists) 0)
        (dolist (l (mapcar #'length lists))
          (setf res (= l (length (car lists))))))
      res)))
