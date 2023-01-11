;linear algebra closest approximation 

;form of linear algebra:  '(plus mult dot)

;polynomial vector space

;polynomial addition
(defun vplus (x y)
	(if (or (null x) (null y))
		(or x y)
		(cons 
			(+ (car x) (car y))
			(vplus (cdr x) (cdr y)))))
;scalar multiplication
(defun *p (a poly)
	(and
		poly
		(cons (* a (car poly)) (*p a (cdr poly)))))
;polynomial dot product
(defun ptimes (poly1 poly2)
	(if (null poly1)
		nil
		(vplus 
			(*p (car poly1) poly2)
			(cons 0 (ptimes (cdr poly1) poly2)))))
(defun evalu (poly x)
	(if (null poly) 
		0
		(+ (car poly) (evalu (*p x (cdr poly)) x))))
(defun integp1 (poly)
	(and
		poly
		(cons 
			(/ (car poly) (length poly))
			(integp1 (cdr poly)))))
(defun integp (poly)
	(let ((rev (reverse poly)))
		(let ((intgrl (integp1 rev)))
			(cons 0 (reverse intgrl)))))
(defun integr (poly)
	(let ((a (integp poly)))
		(- (evalu a pi) (evalu a (* -1 pi)))))
(defun pdot (poly1 poly2)
	(let ((prod (ptimes poly1 poly2)))
		(integr prod)))
(defun integ (fun n)
	(if (= n 0)
		0
		(let ((x (- pi (/ (* 2 pi n) 1000))))
			(+ 
				(* (/ (* 2 pi) 1000) (funcall fun x))
				(integ fun (- n 1))))))
(setf polyn (list #'vplus #'*p #'pdot))





