(define set?
  (lambda (l)
    (cond
     ((null? l) #t)
     (else
      (cond 
       ((member? (car l) (cdr l)) #f) 
       (else (set? (cdr l))))))))
  

(define makeset
  (lambda (l)
    (cond
     ((null? l) '())
     ((member? (car l) (cdr l)) 
      (makeset (cdr l)))
     (else
      (cons (car l) (makeset (cdr l)))))))

(define makeset
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (car l) (makeset (multirember (car l) (cdr l))))))))


(define subset?
  (lambda (set1 set2)
    (cond 
     ((null? set1) #t)
     (else 
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))
      

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else 
      (cond 
       ((member? (car set1) set2) #t)
       (else
        (intersect? (cdr set1) set2)))))))
       

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else 
      (or  
       (member? (car set1) set2)
        (intersect? (cdr set1) set2))))))
       

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) 
      (cons (car set1) (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))


(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1) (union (cdr set1) set2))))))


(define intersectall 
  (lambda (set-l)
    (cond
     ((null? (cdr set-l)) (car set-l))
     (else
      (intersect (car set-l)  (intersectall (cdr set-l)))))))
     

(define a-pair?
  (lambda (x)
    (cond
     ((null? x) #f)
     ((atom? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))


(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (x y)
    (cons x (cons y '()))))

(define a-pair
  (lambda (x y)
    (cons x (cons y '()))))


(define fun?
  (lambda (rel)
    (set? (firsts rel))))


(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons 
       (a-pair (second (car rel)) (first (car rel))) 
       (revrel (cdr rel)))))))
      

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (cdr  (car l))) (seconds (cdr l)))))))



(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(define one-to-one?
  (lambda (rel)
    (fun? (revrel rel))))
