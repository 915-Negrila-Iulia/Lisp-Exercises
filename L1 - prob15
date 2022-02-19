; L1 - problem 15

; a)
; insert element E on given position n
; in linear list L

(defun insert4 (l e n pos col)
  (cond
      ((null l) col)
      ((equal pos n) (insert4 (cdr l) e n (+ pos 1) (append col (list e (car l)))))
      (t (insert4 (cdr l) e n (+ pos 1) (append col (list (car l)))))
   )
)

; b)
; return sum S of all numerical atoms of a list L
; at any level

(defun sum4 (l)
  (cond 
      ((null l) 0)
      ((numberp (car l)) (+ (car l) (sum4 (cdr l))))
      ((listp (car l)) (+ (sum4 (car l)) (sum4 (cdr l))))
      (t (sum4 (cdr l)))
   )
)

; c)
; Write a function to return the set of all sublists of a given linear list.
; Ex. For list ((1 2 3) ((4 5) 6)) => ((1 2 3) (4 5) ((4 5) 6)) 

(defun sublists4 (l)
   (cond
	((null l) nil)
        ((not (listp (car l))) (sublists4 (cdr l)))
        (t (cons (car l) (append (sublists4 (car l)) (sublists4 (cdr l)))))
    )
)

; d)
; Write a function to test the equality of two sets, without using the difference of two sets.

(defun find4 (e l)
   (cond
       ((null l) nil)
       (t (or (equal (car l) e) (find4 e (cdr l))))
   )
)

(defun eqSets4 (l p)
  (cond
      ((null l) T)
      (t (and (find4 (car l) p) (eqSets4 (cdr l) p)))
   )
)
