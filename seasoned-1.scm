(define is-first?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (eq? (car lat) a)))))


(define two-in-a-row?
  (lambda (lat)
    (cond 
     ((null? lat) #f)
     (else 
      (or (is-first? (car lat) (cdr lat))
      (two-in-a-row? (cdr lat)))))))
     



(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat)  #f)
     (else
      (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else 
      (or (eq? (car lat) a)
          (two-in-a-row?  lat))))))


(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? preceding (car lat))
          (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
     (two-in-a-row-b? (car lat) (cdr lat))))))




; 結果リストの１つ前の数と入力リストの現在の数を足したリストを結果とする。
(define sum-of-prefixes-b
  (lambda (sum tup)
    (cond
     ((null? tup) '())
     (else
      (cons (+ sum (car tup)) 
            (sum-of-prefixes-b (+ sum (car tup))
                               (cdr tup) ))))))


(define sum-of-prefixes
  (lambda (tup)
    (cond
     ((null? tup) '())
     (else
      (sum-of-prefixes-b 0 tup)))))
; ;;;;

; 数のリストをとり、
; それぞれの数だけさかのぼった位置にある数のリストを返す
(define pick
  (lambda (n lat)
    (cond
     ((eq? n 1) (car lat))
     (else
      (pick (- n 1) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
     ((null? tup) '())
     (else
      (cons (pick (car tup)  (cons (car tup) rev-pre))
            (scramble-b (cdr tup) 
                        (cons (car tup) rev-pre)))))))
                  
(define scramble
  (lambda (tup)
    (cond
     ((null? tup) '())
     (else
      (scramble-b tup '())))))