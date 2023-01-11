(load "InnerProductSpace.lisp")
(load "PolynomialVectorSpace.lisp")

;sine polynomial
 
(defun sinep1 (n)
	(if (= n 0)
		(cons 0 nil)
		(if (evenp n)
		(cons 0 (sinep1 (- n 1)))
			(if (= (mod n 4) 1)
				(cons (/ 1 (! n)) (sinep1 (- n 1)))
				(cons (* -1 (/ 1 (! n))) (sinep1 (- n 1)))))))
(defun sinep (n)
	(let ((a (sinep1 n)))
		(reverse a)))


;----------------------------------------------------------------------------------------
;sine approximation polynomial

(setf sine (sinep 30))
(setf pbasis '(
	(0 1)
	(0 0 0 1)
	(0 0 0 0 0 1)))
(setf orthob (gsortho polyn pbasis))
(setf approx (projorthobasis polyn orthob sine))
(print approx)