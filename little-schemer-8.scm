(define multirember-f 
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) 
      (multirember-f test? a (cdr l)))
     (else
      (cons (car l) (multirember-f test? a (cdr l)))))))


(define rember-f 
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) 
      (cdr l))
     (else
      (cons (car l) (rember-f test? a (cdr l)))))))
      
      
(define rember-f 
  (lambda (test?)
    (lambda (a l)
      (cond 
       ((null? l) '())
       ((test? a (car l)) 
        (cdr l))
       (else
        (cons (car l) (rember-f test? a (cdr l))))))))

(define rember-eq
  (rember-f eq?))


(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       (else (cond
              ((test? old (car l)) (cons old (cons new (cdr l))))
              (else (cons (car l) (insertR-f new old (cdr l))))))))))


(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       (else (cond
              ((test? old (car l)) (cons new (cons old (cdr l))))
              (else (cons (car l) (insertL-f new old (cdr l))))))))))


(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))


(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       (else (cond
              ((eq? old (car l)) (seq new old (cdr l)))
              (else (cons (car l) ((insert-g seq) new old (cdr l))))))))))


(define insertL
  (insert-g 
   (lambda (new old l)
     (cons new (cons old l)))))
   

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))


(define subst
  (lambda (new old l)
    ((insert-g seqS) new old l)))

(define rember
  (lambda (new old l)
    ((insert-g 
      (lambda (new old l) l)) #f old l)))
      


(define multirember-f 
  (lambda (test?)
    (lambda (a l)
      (cond 
       ((null? l) '())
       (else
        (cond
         ((test? a (car l)) 
          ((multirember-f test?) a (cdr l)))
         (else
          (cons (car l) ((multirember-f test?) a (cdr l))))))))))

(define multirember-eq?
  (multirember-f eq?))

(define multiremberT
  (lambda (test? l)
    (cond
     ((null? l) '())
     (else
      (cond
       ((test? (car l)) (multiremberT test? (cdr l)))
       (else
        (cons (car l) (multiremberT test? (cdr l)))))))))


(define multirember&co
  (lambda (a l col)
    (cond
     ((null? l) 
      (col '() '()))
     ((eq? (car l) a)
      (multirember&co 
       a (cdr l)
       (lambda (newl seen)
         (col newl 
              (cons (car l) seen)))))
     (else
      (multirember&co 
       a (cdr l)
       (lambda (newl seen)
         (col (cons (car l) newl) seen)))))))


(define a-friend 
  (lambda (x y)
    (null? y)))
      

(define new-friend
  (lambda (newl seen)
    (a-friend newl
              (cons '(tune) seen))))


(define last-friend
  (lambda (x y)
    (length x)))
                      
      

          
(define multiinsertLR
  (lambda (new oldL oldR l)
    (cond
     ((null? l) '())
     ((eq? oldL (car l)  )
      (cons new
            (cons (car l) 
                  (multiinsertLR new oldL oldR (cdr l)))))
     ((eq? oldR (car l))
      (cons (car l)
            (cons new 
                  (multiinsertLR new oldL oldR (cdr l)))))
     (else 
      (cons (car l) 
            (multiinsertLR new oldL oldR (cdr l)))))))
                 
     

(define multiinsertLR&co
  (lambda (new oldL oldR l col)
    (cond
     ((null? l)  (col '() 0 0))
     ((eq? oldL (car l) )
      (multiinsertLR&co new oldL oldR (cdr l)  
                     (lambda (newlat L R)
                       (col
                        #?=(cons new (cons (car l) newlat))
                        (+ L 1)
                        R))))
     ((eq? oldR (car l))
      (multiinsertLR&co new oldL oldR (cdr l)
                    (lambda (newlat L R)
                      (col
                       #?=(cons (car l) (cons new newlat))
                       L
                       (+ R 1)))))
     (else
      (multiinsertLR&co new oldL oldR (cdr l)
                     (lambda (newlat L R)
                       (col
                       #?=(cons (car l) newlat)
                        L
                        R)))))))
                           
            
      
            
(define evens-only*
  (lambda (l)
    (cond
     ((null? #?=l) '())
     ((atom? #?=(car l))
      (cond 
       ((even? (car l)) 
        (cons (car l) (evens-only* (cdr l))))
       (else
        (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
            (evens-only* (cdr l)))))))
       

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond ((even? (car l)) 
             (evens-only*&co 
              (cdr l) 
              (lambda (newl p s)
                (col  (cons (car l) newl)
                      (* (car l) p)
                      s))))
            (else
             (evens-only*&co 
              (cdr l) 
              (lambda (newl p s)
                (col 
                 newl
                 p
                 (+ (car l) s )))))))

     (else
      (evens-only*&co (car l)
                      (lambda (newl p s)
                        (evens-only*&co
                         (cdr l)
                         (lambda (newl2 p2 s2)
                           (col (cons newl newl2)
                                (* p p2)
                                (+ s s2))))))))))


(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 1 0))
          ((atom? (car l))
           (cond ((even? (car l))
                  (evens-only*&co
                   (cdr l)
                   (lambda (newl p s)
                     (col (cons (car l) newl)
                          (* (car l) p)
                          s))))
                 (else (evens-only*&co
                        (cdr l)
                        (lambda (newl p s)
                          (col newl p (+ s (car l))))))))
           (else (evens-only*&co
                  (car l)
                  (lambda (al ap as)
                    (evens-only*&co (cdr l)
                                    (lambda (dl dp ds)
                                      (col (cons al dl)
                                           (* ap dp)
                                           (+ as ds))))))))))

(define the-last-friend
    (lambda (newl product sum)
      (cons sum (cons product newl))))

       