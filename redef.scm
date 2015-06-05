; リスト をつなげる
;(define append
;  (lambda (lst lst+)
;    (if (null? lst)
;        '()
;        (cons (car lst) 
;                   (append (cdr lst)  lst+)))))
; リスト要素数
;(define length
;  (lambda (lst)
;    (if (null? lst)
;        0
;        (+ 1 (length (cdr lst))))))

; リストから最初のn要素を除いた部分リスト
(define list-tail
  (lambda (lst n)
    (if (zero? n)
        lst
        (list-tail (cdr lst) (- n 1)))))
        
(define list-ref
  (lambda (lst n)
    (if (zero? n)
        (car lst)
        (list-ref (cdr lst) (- n 1)))))

; リストから最初のn要素の部分リスト
(define list-head
  (lambda (lst n)
    (if (zero? n)
        '()
        (cons (car lst) (list-head (cdr lst) (- n  1) )))))
; リストから最初のn要素の部分リスト（末尾再帰）
(define list-head
  (lambda (lst n)
    (define list-head-iter
      (lambda (lst n r)
        (if (zero? n) r
            (list-head-iter (cdr lst)
                            (- n 1)
                            (cons (car lst) r)))))
    (reverse (list-head-iter lst n '()))))
             

; map の再定義
(define map-unit
  (lambda (proc lst)
    (if (null? lst)
        '()
        (cons (proc (car lst))
                    (map-unit proc (cdr lst) )))))
(define map-mult
  (lambda (proc rest)
    (if (member '() rest)
        '()
        (cons (apply proc (map-unit car rest))
              (apply map proc (map-unit cdr rest))))))
(define map
  (lambda (proc . rest)
    (if (null? (cdr rest))
               (map-unit proc (car rest))
               (map-mult proc rest))))
                     
(define for-each
  (lambda (proc lst)
    (if (null? lst)
        '<unspecified>
        (begin (proc (car lst))
              (for-each proc (cdr lst))))))