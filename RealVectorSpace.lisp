
;R^n vector space

(defun vdot (x y)
	(if (or (equal x nil) (equal y nil))
		0
		(+ (* (car x) (car y)) (vdot (cdr x) (cdr y)))))
(setf vect (list #'vplus #'*p #'vdot))