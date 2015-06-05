; 2つの集合の積
(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond 
               ((null? set) '())
               (else 
                (cond 
                 ((member? (car set) set2)
                  (cons (car set) 
                        (I (cdr set))))
                 (else
                  (I (cdr set)))))))))
      (I set1))))
               

; 集合のリストからその全集合の積
; 
(define intersectall
  (lambda (lset)
    (cond
     ((null? lset) '())      
     ((null? (cdr lset)) (car lset))
     (else
       (intersect (car lset) 
                  (intersectall 
                   (cdr lset)))))))

; 集合のリストからその全集合の積
; (letrecを使う)

(define intersectall
  (lambda (lset)
    (letrec 
        ((A (lambda (lset)
              (cond
               ((null? (cdr lset)) (car lset))
               (else
                (intersect (car lset) 
                           (intersectall 
                            (cdr lset))))))))
      (cond 
       ((null? lset) '())
       (else
        (A lset))))))


; 集合のリストからその全集合の積
; (letrecを使う)
; (letccを使う.. 空集合があればすぐに結果として空集合を返す )
(define intersectall
  (lambda (lset)
    (let/cc hop
           (letrec 
               ((A (lambda (lset)
                     (cond
                      ((null? (car lset)) hop '())
                      ((null? (cdr lset)) (car lset))
                      (else
                       (intersect (car lset) 
                                  (A
                                   (cdr lset))))))))
             (cond 
              ((null? lset) '())
              (else
               (A lset)))))))
           


; 集合のリストからその全集合の積
; (letrecを使う)
; (let/ccを使う.. 2集合の積もletrec で定義して、結果が空になる場合は、すぐに結果が返されるようにする)
(define intersectall
  (lambda (lset)
    (let/cc hop
           (letrec 
               (
                (A (lambda (lset)
                     (cond
                      ((null? (car lset)) hop '())
                      ((null? (cdr lset)) (car lset))
                      (else
                       (I (car lset) 
                                  (A
                                   (cdr lset)))))))
                (I (lambda (set1 set2)
                      (letrec
                          ((J (lambda (set)
                                (cond 
                                 ((null? set) ())
                                 (else 
                                  (cond 
                                   ((null? set) '())
                                   ((member? (car set) set2)
                                    (cons (car set) 
                                          (J (cdr set))))
                                   (else
                                    (J (cdr set)))))))))
                        (cond
                         ((null? set2) hop '())
                         (else (J set1)))))))
                
             (cond 
              ((null? lset) '())
              (else
               (A lset)))))))


; lat から指定した要素を削除する。
(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat) '())
               (else
                (cond
                 ((eq? (car lat) a) 
                  (R (cdr lat)))
                 (else
                  (cons (car lat) 
                        (R (cdr lat))))))))))
      (R lat))))
           

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat) '())
               (else
                (cond
                 ((eq? (car lat) a) 
                  '()
                 (else
                  (cons (car lat) 
                        (R (cdr lat))))))))))
      (R lat))))


; lat から指定した要素以前を削除する
; l
(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
            (letrec 
                ((R (lambda (lat)
                      (cond
                       ((null? lat) '())
                       (else
                        (cond
                         ((eq? (car lat) a)
                          (skip (R (cdr lat))))
                         (else
                          (cons (car lat)
                                (R (cdr lat))))))))))
              (R lat)))))
                                
                         
                        
     
        
           
