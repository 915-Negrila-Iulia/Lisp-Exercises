; L2 - problem 4

; Convert a tree of type (2) to type (1).

;     A
;    / \
;   B   C
;      / \
;     D   E

; (1)  (A 2 B 0 C 2 D 0 E 0)
; (2)  (A (B) (C (D) (E)))

; compute number of elements of a given list -1 
(defun count4 (l)
  (cond
      ((null l) -1)
      (t (+ 1 (count4 (cdr l))))
  )
)

;convert tree (2)->(1)
(defun plm (l)
  (cond 
      ((ATOM l) NIL)
      (T (APPEND (LIST (CAR l)) (LIST(count4 l)) (APPLY #'APPEND (MAPCAR #'plm L))))
   
  )
)

