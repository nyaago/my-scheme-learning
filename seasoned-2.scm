;;
(define multirember
  (lambda (a lat)
    ((letrec 
         ((mr (lambda (lat)
                (cond
                 ((null? lat) '())
                 ((eq? a (car lat))
                  (mr (cdr lat)))
                 (else
                  (cons (car lat) (mr (cdr lat))))))))
     mr)
    lat)))


;; function を指定して latから条件にあったものを除外する
; 関するを返す関数
(define multirember-f
  (lambda (test?)
    (letrec 
        ((mr-f
          (lambda (a lat)
            (cond
             ((null? lat) '())
             ((test? a (car lat))
              (mr-f a (cdr lat)))
             (else
              (cons (car lat) (mr-f a (cdr lat))))))))
      mr-f)))


; letrec を使って再帰する member? （テスト値がlat中にあるか?）
(define member?
  (lambda (a lat)
    ((letrec
        ((yes? 
          (lambda (lat)
            (cond
             ((null? lat) #f)
             ((eq? (car lat) a) #t)
             (else
              (yes? (cdr lat)))))))
      yes?) lat)))

; または,,
(define member?
  (lambda (a lat)
    (letrec
        ((yes? 
          (lambda (lat)
            (cond
             ((null? lat) #f)
             ((eq? (car lat) a) #t)
             (else
              (yes? (cdr lat)))))))
      (yes? lat))))



; letrec を使って再帰する ２つの集合の和を返す関数
(define union
  (lambda (set1 set2)
    ((letrec
         ((U (lambda (set)
             (cond
              ((null? set) set2)
              ((member? (car set) set2 )
               (U (cdr set)))
              (else
               (cons (car set) (U (cdr set))) )))))
     U) set1)))
; または
(define union
  (lambda (set1 set2)
    (letrec
         ((U (lambda (set)
             (cond
              ((null? set) set2)
              ((member? (car set) set2 )
               (U (cdr set)))
              (else
               (cons (car set) (U (cdr set))) )))))
     (U set1))))


; リストのなかでおなじアトムが２回連続して出てくるか？
(define two-in-a-row?
  (lambda (lat)
    (letrec 
        ((W (lambda (a lat)
              (cond
               ((null? lat) #f)
               (else 
                (or
                 (eq? (car lat) a)
                 (W (car lat) (cdr lat))))))))
      (cond 
       ((null? lat) #f)
       (else
        (W (car lat) (cdr lat)))))))
      

; 結果リストの１つ前の数と入力リストの現在の数を足したリストを結果とする。
(define sum-of-prefixes
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond 
               ((null? tup) '())
               (else
                (cons (+ sss (car tup))
                      (S (+ sss (car tup)) 
                         (cdr tup))))))))
      (S 0 tup))))


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
                (cons (pick (car tup) (cons (car tup) rp))
                      (P (cdr tup) 
                         (cons (car tup) rp))))))))
      (cond
       ((null? tup) '())
       (else 
        (P tup '()))))))


(define scramble-b
  (lambda (tup rev-pre)
    (cond
     ((null? tup) '())
     (else
      (cons (pick (car tup)  (cons (car tup) rev-pre))
            (scramble-b (cdr tup) 
                        (cons (car tup) rev-pre)))))))
                  

              


