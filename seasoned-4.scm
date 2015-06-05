(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2) #t))
     ((or  (null? l1) (null? l2) #f))
     (else
      (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))


; 一番左のatom
; let .
(define leftmost
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (car l))
     (else
      (let ((a  (leftmost (car l))))
        (cond    
         ((atom? a) a)
         (else
          (leftmost (cdr l)))))))))


; S式から指定されたatomを除く
(define rember1*
  (lambda (a l)
    (letrec 
        ((R (lambda (l)
              (cond 
               ((null? l) '())
               ((atom? (car l))
                (cond 
                 ((eq? a (car l))
                  (cdr l))
                 (else
                  (cons (car l) 
                        (R (cdr l))))))
               (else
                (cond
                 ((eqlist?  (R (car l))
                            (car l))
                  (cons (car l) (R (cdr l))))
                 (else
                  (cons (R (car l)) (cdr l)))))))))
      (R l))))


; S式から指定されたatomをの除いたリスト
; let ..
(define rember1*
  (lambda (a l)
    (letrec 
        ((R (lambda (l)
              (cond 
               ((null? l) '())
               ((atom? (car l))
                (cond 
                 ((eq? a (car l))
                  (cdr l))
                 (else
                  (cons (car l) 
                        (R (cdr l))))))
               (else
                (let ((av (R (car l))))
                         (cond
                          ((eqlist?  av (car l))
                           (cons (car l) (R (cdr l))))
                          (else
                           (cons av (cdr l))))))))))
      (R l))))


 ; list の階層の深さ
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l))
      (depth* (cdr l)))
     (else
      (let ((a (+ (depth* (car l)) 1))
            (d (depth* (cdr l))))
        (cond
         ((> a d) 
          a)
         (else 
          d)))))))



(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l))
      (depth* (cdr l)))
     (else
      (max (+ (depth* (car l)) 1)
            (depth* (cdr l)))))))
         


; 数のリストをとり、
; それぞれの数だけさかのぼった位置にある数のリストを返す
(define pick
  (lambda (n lat)
    (cond
     ((eq? n 1) (car lat))
     (else
      (pick (- n 1) (cdr lat))))))

(define scramble
  (lambda (tup)
    (letrec 
        ((P (lambda (tup rp)
              (cond
               ((null? tup) '())
               (else
                (let ((rp (cons (car tup) rp)))
                  (cons (pick (car tup) rp)
                        (P (cdr tup) 
                           rp))))))))
      (cond
       ((null? tup) '())
       (else 
        (P tup '()))))))
         

; 一番左のatom
; let/cc でぬける
(define leftmost
  (lambda (l)
    (letrec 
        ((lm (lambda (l out)
               (cond
                ((null? l) '())
                ((atom? (car l)) 
                 (out (car l)))
                (else
                 (lm (car l) out)
                 (lm (cdr l) out))))))
      (let/cc skip
              (lm l skip)))))
                 

(define leftmost
  (lambda (l)
    (let/cc skip
            (letrec 
                ((lm (lambda (l out)
                       (cond
                        ((null? l) '())
                        ((atom? (car l)) 
                         (out (car l)))
                        (else
                         (lm (car l) out)
                         (lm (cdr l) out))))))
              (lm l skip)))))

               
      

(define leftmost
  (lambda (l)
    (let/cc skip
            (letrec 
                ((lm (lambda (l)
                       (cond
                        ((null? l) '())
                        ((atom? (car l)) 
                         (skip (car l)))
                        (else
                         (lm (car l))
                         (lm (cdr l) ))))))
              (lm l)))))


; S式から指定されたatomをの除いたリスト               
;

(define  rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l)
                (rm a (cdr l) oh))))
     (else
      (if (atom? 
           (let/cc oh 
                   (rm a (car l) oh)))
          (cons (car l) 
                (rm a (cdr l) oh))
          (cons (rm a (car l) 0)
                (cdr l)))))))

(define  rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l)
                (rm a (cdr l) oh))))
     (else
      (let ((new-car
           (let/cc oh 
                   (rm a (car l) oh))))
        (if (atom? new-car)
          (cons (car l) 
                (rm a (cdr l) oh))
          (cons new-car
                (cdr l))))))))


(define rember1*
  (lambda (a l)
    (let ((new-l 
           (let/cc oh (rm a l oh))))
      (if (atom? new-l)
          l
          new-l))))
                 



           
            
             
    
