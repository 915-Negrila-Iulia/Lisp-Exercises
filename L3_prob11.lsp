; L3 - prob11
; Determine the depth of a list

(defun depth4 (l)
  (cond
      ((ATOM l) 0)
      (T (+ 1 (APPLY #'MAX (MAPCAR #'depth4 l))))
  )
)

