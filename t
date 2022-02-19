
(defun myfunction(l)
  (mapcon #'list l)
)

(defun main()
  (mapcon #'myfunction '(1 2))
)

(defun rem4(l)
  (cond 
      ((and(numberp l)(= 0 (mod l 3))) nil)
      ((atom l) (list l))
      ;(t (list(mapcan #'rem4 l)))
      (t (list (apply #'append(mapcar #'rem4 l))))
  )
)