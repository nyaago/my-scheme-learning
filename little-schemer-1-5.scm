(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat? 
  (lambda (l)
    (cond 
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f)
)))


(define member?
  (lambda (x l)
    (cond
     ((null? l) #f)
     ((eq? x (car l)) #t)
     (else (member? x (cdr l)))
)))


(define member?
  (lambda (x l)
    (cond
     ((null? l) #f)
     (else (or (eq? x (car l))
               (member? x (cdr l))))
)))


(define rember
  (lambda (x l)
    (cond
     ((null? l) '())
     (else (cond 
            ((eq? x (car l)) (cdr l))
;            ((eq? x (car l))  (rember x (cdr l)))
            (else (cons (car l)  (rember x (cdr l))))))
)))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l))))
)))


(define insertR
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((eq? old (car l)) (cons old (cons new (cdr l))))
     (else (cons (car l) (insertR new old (cdr l))))
)))


(define insertR
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? old (car l)) (cons old (cons new (cdr l))))
            (else (cons (car l) (insertR new old (cdr l)))))
))))


(define insertL
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? old (car l)) (cons new (cons old (cdr l))))
            (else (cons (car l) (insertL new old (cdr l)))))
))))


(define insertL
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? old (car l)) (cons new  l))
            (else (cons (car l) (insertL new old (cdr l)))))
))))

(define subst
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? old (car l)) (cons new (cdr l)))
            (else (cons (car l) (subst new old (cdr l)))))
))))


(define multirember
  (lambda (x l)
    (cond
     ((null? l) '())
     (else (cond 
            ((eq? x (car l))  (multirember x (cdr l)))
            (else (cons (car l)  (multirember x (cdr l))))))
)))

(define multiinsertR
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? old (car l)) 
             (cons (car l) 
                   (cons new 
                         (multiinsertR new old (cdr l)))))
            (else (cons (car l) 
                        (multiinsertR new old (cdr l))))
)))))


(define multiinsertL
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? old (car l)) 
             (cons new 
                   (cons (car l) 
                         (multiinsertL new old (cdr l)))))
            (else (cons (car l) 
                        (multiinsertL new old (cdr l))))
)))))



(define multisubst
  (lambda (new old l)
    (cond
     ((null? l) '())
     (else (cond
            ((eq? old (car l))
             (cons new (multisubst new old (cdr l))))
            (else (cons (car l) (multisubst new old (cdr l))))
)))))



(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))



(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (+ (car tup) (addtup (cdr tup))))
)))


(define x
  (lambda (n m)
    (cond 
     ((zero? m) 0)
     (else (+ n (x n  (sub1 m))))
)))


(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))
))))


(define expr 
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (x n (expr n (sub1 m))))
)))


(define %
  (lambda (n m)
    (cond
     ((<  n m)  0)
     (else (+ (% (- n m) m) 1))
)))


(define length 
  (lambda (l)
    (cond 
     ((null? l) 0)
     (else (add1 (length (cdr l))))
)))
      
(define pick
  (lambda (n l)
    (cond
     ((zero? (sub1 n))  (car l))
     (else (pick (sub1 n) (cdr l)))
)))


(define rempick
  (lambda (n l)
    (cond
     ((zero? (sub1 n)) (cdr l))
     (else (cons (car l) (rempick (sub1 n) (cdr l)) ))
)))


(define no-nums
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cond 
       ((number? (car l)) (no-nums (cdr l)))
       (else (cons (car l) (no-nums (cdr l))))
)))))

(define all-nums
  (lambda (l)
    (cond 
     ((null? l) '())
     (else
      (cond
       ((number? (car l)) 
        (cons (car l) (all-nums (cdr l))))
       (else (all-nums (cdr l))))
))))
                 

(define eqan?
  (lambda (a1 a2)
    (cond 
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2))
)))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond 
       ((eq? a (car lat))  (+ 1 (occur a (cdr lat))))
       (else (occur a (cdr lat)))
)))))
            
(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
     ((one?  n)  (cdr lat))
     (else (cons (car lat) (rempick (- n 1) (cdr lat))))
)))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond 
       ((eq? a  (car l)) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

(define insertR* 
  (lambda (new old l)
    (cond 
     ((null? l) l)
     ((atom? (car l))
      (cond
       ((eq? old (car l)) 
        (cons (car l) (cons new (insertR* new old  (cdr l)))))
       (else 
        (cons (car l) (insertR* new old  (cdr l))))))
     (else (cons (insertR* new old (car l)) 
                (insertR* new old (cdr l)))))))
     
      


(define insertR*
  (lambda (new old l)
    (cond ((null? l) l)
          ((atom? (car l)) 
           (cond 
            ((eq? old (car l)) 
             (cons (car l) (cons new  (insertR* new old (cdr l)))))
            (else 
             (cons (car l) (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))))))

  
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l)  a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else
      (+ (occur* a (car l))
         (occur* a (cdr l)))))))
      
       

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) 
        (cons new (subst* new old (cdr l))))
       (else 
        (cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l))
            (subst* new old (cdr l)))))))
      

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) 
        (cons new (cons old (insertL* new old (cdr l)))))
       (else 
        (cons (car l) (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l))
            (insertL* new old (cdr l)))))))
      
(define member*
  (lambda (a l)
    (cond 
     ((null? l) #f)
     ((atom? (car l))
      (or
       (eq? (car l) a)
       (member* a (cdr l))))
     (else
      (or (member* a (car l))
         (member* a (cdr l)))))))




(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2) #t))
     ((or  (null? l1) (null? l2) #f))
     (else
      (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eq? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else 
      (eqlist? s1  s2)))))


(define rember
  (lambda (s l)
     (cond 
      ((null? l) '())
      ((equal? s (car l)) (cdr l))
      (else 
       (cons (car l)  (rember s (cdr l)))))))


