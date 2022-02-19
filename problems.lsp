
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

(defun sum4 (l)
  (cond
      ((null l) 0)
      ((numberp (car l)) (+ (car l) (sum4 (cdr l))))
      ((listp (car l)) (+ (sum4 (car l)) (sum4 (cdr l))))
      (t (sum4 (cdr l)))
  )
)

(defun addEnd(e l)
  (cond
      ((null l) (list e))
      (t (cons (car l) (addEnd e (cdr l))))
  )
)

(defun reverse4(l)
  (cond
      ((null l) nil)
      (t (append (reverse4 (cdr l)) (list (car l))))
  )
)

(defun reverseCol(l col)
  (cond
      ((null l) col)
      (t (reverseCol (cdr l) (cons (car l) col)))
  )
)

(defun reverseMain(l)
  (reverseCol l nil)
)

(defun pairListCol(e l col)
  (cond
      ((null l) col)
      (t (pairlistCol e (cdr l) (cons (list e (car l)) col)))
  )
)

(defun pairListMain(e l)
  (pairListCol e l nil)
)

(defun pairList(e l)
  (cond
      ((null l) nil)
      (t (cons (list e (car l)) (pairList e (cdr l))))
  )
)

(defun pairCondList(e l)
  (cond
      ((null l) nil)
      ((< e (car l)) (cons (list e (car l)) (pairCondList e (cdr l))))
      (t (pairCondList e (cdr l)))
  )
)

(defun pairElems(l)
  (cond
     ((null l) nil)
     (t (append (pairCondList (car l) (cdr l)) (pairElems (cdr l))))
  )
)

(defun doubleNum(l)
  (cond 
      ((null l) nil)
      ((numberp (car l)) (cons (* 2 (car l)) (doubleNum (cdr l))))
      ((atom (car l)) (cons (car l) (doubleNum (cdr l))))
      (t (cons (doubleNum (car l)) (doubleNum (cdr l))))
  )
)

(defun dublare(l)
  (cond
      ((numberp l) (* 2 l))
      ((atom l) l)
      (t (cons (dublare (car l)) (dublare (cdr l))))
  )
)

(defun firstAtomSurface(l)
  (cond
     ((null l) nil)
     ((atom (car l)) (car l))
     (t (firstAtomSurface (cdr l)))
  )
)

(defun firstAtomAnyLvL(l)
  (cond
     ((null l) nil)
     ((atom (car l)) (car l))
     (t (firstAtomAnyLvL (car l)))
  )
)

(defun sumOfn(n l)
  (cond
      ((null l) 0)
      ((= n 0) 0)
      (t (+ (car l) (sumOfn (- n 1) (cdr l))))
   )
)

(defun nonNumAppend(l)
  (cond
      ((null l) nil)
      ((numberp (car l)) (nonNumAppend(cdr l)))
      ((atom (car l)) (cons (car l) (nonNumAppend(cdr l))))
      (t (append (nonNumAppend (car l)) (nonNumAppend (cdr l))))
  )
)

(defun nonNumCons(l)
  (cond
      ((null l) nil)
      ((numberp (car l)) (nonNumCons(cdr l)))
      ((atom (car l)) (cons (car l) (nonNumCons(cdr l))))
      (t (cons (nonNumCons (car l)) (nonNumCons (cdr l))))
  )
)

(defun insertPos(e p l)
  (cond
      ((= p 1) (cons e l))
      (t (cons (car l) (insertPos e (- p 1) (cdr l))))
   )
)

(defun insReturnSet(e p l)
  (cond
      ((= p 0) nil)
      (t (cons (insertPos e p l) (insReturnSet e (- p 1) l)))
   )
)

(defun insertion(e l)
      (insReturnSet e (+ (length l) 1) l)
) 

(defun inorder(l)
  (cond
      ((null l) nil)
      (t (append (inorder (cadr l)) (list (car l)) (inorder (caddr l))))
   )
)

(defun postorder(l)
  (cond 
      ((null l) nil)
      (t (append (postorder (cadr l)) (postorder (caddr l)) (list (car l))))
   )
)

(defun preorder(l)
  (cond
      ((null l) nil)
      (t (append (list (car l)) (preorder (cadr l)) (preorder (caddr l))))
   )
)

(defun doubleNum(l)
  (cond
      ((numberp l) (* 2 l))
      ((atom l) l)
      (t #'MAPCAR (doubleNum l))
   )
)

(defun mulEach(l1 l2)
  (cond
      ((and (atom l1) (atom l2)) (list(* l1 l2)))
      (t (apply #'append (MAPCAR #'mulEach l1 l2)))
  )
)

(defun mainMat(m1 m2)
  (cond 
      ((and (atom m1) (atom m2)) nil)
      ((and (listp m1) (listp m2)) (append (mulEach m1 m2) (apply #'append (MAPCAR #'mainMat m1 m2))))
      (t (MAPCAR #'mainMat m1 m2))
   )
)

(defun atomsRev(l)
  (cond
      ((atom l) (list l))
      (t (MAPCAN #'atomsRev(reverse l)))
  )
)

(defun nrOcc(e l)
  (cond
       ((equal l e) 1)
       ((atom l) 0)
       (t (apply #'+ (MAPCAR #'(LAMBDA(l) (nrOcc e l)) l)))
   )
)

(defun nrap(e L)
  (cond 
      ((equal L e) 1) 
      ((atom L) 0) 
      (t (apply #'+ (mapcar #'(lambda(L) (nrap e L) )l ) ))
  )
)

(defun atomsDepthN(n l)
  (cond
      ;((and (= n 0) (atom l)) (list l))
      ;((= n 0) nil)
      ;((atom l) nil)
      ;(t (MAPCAN #'(LAMBDA(l) (atomsDepthN (- n 1) l)) l))
      ((and (= n 0) (atom L)) (list L))
      ((= n 0) nil) 
      ((atom L) nil) 
      (t (mapcan #'(lambda(l) (atomsDepthN (- n 1) l)) l))
   )
)

(defun lista(L n)
 (cond 
   ((and (= n 0) (atom L)) (list L))
   ((= n 0) nil) 
   ((atom L) nil)
   (t (mapcan #'(lambda(L) (lista L (- n 1))) L))
)
)

(defun subm(L) 
  (cond 
      ((null L) (list nil)) 
      (t ((lambda (s) (append s (mapcar #'(lambda (sb) (cons (car L) sb)) s))) (subm(cdr L) )) 
   )
)
)

(defun permutari(L)
  (cond
     ((null (cdr L)) (list L))
     (t (mapcan #'(lambda (e) (mapcar #'(lambda (p) (cons e p) ) (permutari(remove e L)) )) L ))
 )
) 

(defun addNtoN(l e p n)
  (cond
      ((null l) nil)
      ((equal 0 (mod p n)) (cons e (addNtoN l e (+ 1 p) n)))
      (t (cons (car l) (addNtoN (cdr l) e (+ 1 p) n)))
   )
)

(defun mainAddNtoN (l e n)
      (addNtoN l e 1 n) 
)

(defun evenLevel(l lvl)
  (cond
      ((and (atom l) (= (mod lvl 2) 0)) 1)
      ((atom l) 0)
      (t (apply #'+ (mapcar #'(lambda(l)(evenLevel l (+ 1 lvl))) l)))
   )
)

(defun main(l)
  (evenLevel l 0)
)


(defun atomList(l)
  (cond
      ((numberp l) (list l))
      ((atom l) nil)
      (t (apply #'append(MAPCAR #'atomList l)))
   )
)

(defun checkFirst(l)
  (cond 
      ((null l) nil)
      ((null (atomList l)) nil)
      ((= (car (atomList l)) 5) T)
      (T nil)
  )
)

(defun nrSub(l)
  (cond
      ((atom l) 0)
      ((checkFirst l) (+ 1 (apply #'+ (MAPCAR #'nrSub l))))
      (t (apply #'+ (MAPCAR #'nrSub l)))
   )
)

(defun isMember(l e)
  (cond 
      ((and (atom l) (equal l e)) t)
      ((atom l) nil)
      (t (SOME #'IDENTITY (MAPCAR #'(lambda(l)(isMember l e)) l)))
   )
)

(defun allEqual(l e)
  (cond
      ((and (atom l) (equal l e)) t)
      ((atom l) nil)
      (t (EVERY #'IDENTITY (MAPCAR #'(lambda(l)(allEqual l e)) l)))
   )
)

(defun lista(l n)
  (cond
      ((and (= n 0) (atom l)) (list l))
      ((= n 0) nil)
      ((atom l) nil)
      (t (mapcan #'(lambda(l)(lista l (- n 1))) l))
   )
)

(defun mainL(l n)
  (lista l (+ n 1))
)

(defun exists(l e)
  (cond 
      ((and (atom l) (equal e l)) t)
      ((atom l) nil)
      (t (SOME #'IDENTITY (MAPCAR #'(lambda(l)(exists l e)) l)))
   )
)

(defun depth(l)
  (cond
      ((atom l) 0)
      ((null (cdr l)) 0)
      (t (+ 1 (apply #'max(mapcar #'depth (cdr l)))))
   )
)

(defun EXI(l e n)
  (exists (mainL l n) e)
)

(defun sol(l e n)
  (cond
      ((= n 0) nil)
      ((and (= (mod n 2) 0)(exists (mainL l n) e)) t)
      (t (sol l e (- n 1)))
   )
)

(defun func(l n)
  (cond
      ((and (= (mod n 2) 0) (atom l)) (list 0))
      ((= n 0) nil)
      ((atom l) (list l))
      (t (list(mapcan #'(lambda(l)(modify l (- n 1))) l)))
   )
)

(defun dd(l)
  (- (depth l) 1)
)

(defun fmain(l)
  (func l (- (depth l) 1))
)

(defun modify(l k)
  (cond
      ((and (= k 0)(atom l)) (list 0))
      ((atom l) (list l))
      (t (list(mapcan #'(lambda(l)(modify l (- k 1))) l)))
   )
)

(defun height(l nod col)
  (cond
      ((and (atom l) (equal nod l)) col)
      ((atom l) 0)
      ((null (cdr l)) 0)
      (t (mapcar #'(lambda(l)(height (cdr l) nod (+ col 1))) l))
   )
)

(defun exists(l e)
  (cond
      ((and (atom l) (equal e l)) t)
      ((atom l) nil)
      (t (SOME #'IDENTITY (MAPCAR #'(lambda(l)(exists l e)) l)))
   )
)

(defun path(l e)
  (cond
      ((atom l) nil)
      ((exists l e) (cons (car l) (mapcan #'(lambda(l)(path l e)) (cdr l))))
      (t  (mapcan #'(lambda(l)(path l e)) (cdr l)))
   )
)

(defun pathStruct(l e)
  (cond
      ;((and (atom l) (equal e l)) 0)
      ((atom l) 0)
      ((exists l e) (+ 1 (apply #'+(mapcar #'(lambda(l)(pathStruct l e)) (cdr l)))))
      (t (apply #'+(mapcar #'(lambda(l)(pathStruct l e)) (cdr l))))
   )
)

(defun depthMax(l)
  (cond
      ((atom l) 0)
      ((null (cdr l)) 0)
      (t (+ 1 (apply #'max (mapcar #'depthMax (cdr l)))))
   )
)

(defun main(l e)
  (cond
      ((exists l e) (- (+ 1 (depthMax l))(pathStruct l e)))
      (t -1)
   )
)