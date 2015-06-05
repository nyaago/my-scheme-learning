;; listのfilter
(define (filter pred lis)
  (if (null? lis)
      (list )
      (if (pred (car lis) )
          (cons (car lis)  (filter pred  (cdr lis) ))
          (filter pred  (cdr lis)) )))

; http://d.hatena.ne.jp/mohayonao/20080710/1215674923
;(define (filter-for-tree pred lis)
;  (if (null? lis)
;      (list )
;      (if (pair? (car lis) )
;          ( cons (filter-for-tree pred  (car lis) ) (filter-for-tree pred (cdr lis)) )
;          (if (pred (car lis) )
;              (cons (car lis)  (filter-for-tree pred  (cdr lis) ))
;              (filter-for-tree pred  (cdr lis)) ))) )


;


;; length of list
(define (length lis)
  (if (null? lis)
       0
      (+ (length(cdr lis)) 1) ) )

;; length of list  末尾再帰版      
(define (length lis)
  (define (length-rec lis n)
    (if (null? lis)
      n
      (length-rec (cdr lis) (+ n 1) )
      )
    )
  (length-rec lis 0)
  )

;; リストのreverse  末尾再帰版
;; carで元のリストの最初の要素を取り出し、それを結果リストに加えていくことを繰り返していく。
(define (reverse lis)
  (let reverse-loop ((reversed '() )
                     (src lis))
    (if (pair? src) 
        (reverse-loop (cons (car src)  reversed)  (cdr src) )
        reversed )
    )
)

;(print (reverse '(1 2 3 4 5) ) )

;; マッチした最初の要素を返す
(define (find pred lis)
  (if (pred (car lis) )
            (car lis)
            (find pred (cdr lis) )
      )
)

(print (find (lambda (x) (and (> x 4) (even? x) ) ) '(1 2 3 4 5 6 7 8 ) ) )

;; fold
(define (fold proc ini lis)
  (if (pair?  lis)
      (fold proc (proc (car lis) ini ) (cdr lis))
      ini
      )
)

;; リスト要素の合計を返す
(define (sum lis)
  (let sum-loop ((n 0) 
                 (ls lis))
    (if  (null? ls) 
         n
         (let ( (val (car ls) ) )
           (if (or (real? val) (integer? val) )
               (sum-loop (+ n val) (cdr ls) )
               (sum-loop n (cdr ls) )
           )
        )
     )
   )
)

(define (sum lis)
  (fold (lambda (x t) 
          (+ x t) )
        0
        lis)
)


(print (sum '(1 2 3 4 5) ) )
(print (sum '(1  2.5  3.3 4 5) ) )
;; リスト要素の平均を返す
(define (average lis)
  (let avr-loop ((total 0) 
                 (n 0)
                 (ls lis))
    (if  (null? ls) 
         (/ total n)
         (let ( (val (car ls) ) )
           (if (or (real? val) (integer? val) )
               (avr-loop (+ total val) (+  n 1)  (cdr ls) )
               (avr-loop total (+ n 1)   (cdr ls) )
               )
        )
     )
  )
)
  
(print (average '(1 2 3 4 5) ) )
(print (average '(1  2.5  3.3 4 5) ) )
    


;(print (filter (lambda  (x) (> x 4 ) )  '(1 2 3 4  5 6 8) ))
;(print (length '(1 2 3 4)) )

;; 指定した要素をリストから削除
(define (delete-1 elt args . options)
  (let-optionals* options ((cmp-fn equal?))
    (let delete-loop ((lis1 '())
                      (lis2 args)
                      (org-lis args))
     (cond [(null? lis2) org-lis]
            [(cmp-fn elt (car lis2)) (cons  (cdr lis1) (cdr lis2) ) ]
            [else (delete-loop (cons lis1 (car lis2) ) (cdr lis2) org-lis)] ))))

;; 指定した要素をリストから削除,マッチしたもの全てを削除
(define (delete elt args . options)
  (let-optionals* options ((cmp-fn equal?))
    (let delete-loop ((lis args) )
     (cond [(null? lis) '()]
            [(cmp-fn elt (car lis)) (delete-loop (cdr lis)  )    ]
            [else (cons (car lis)  (delete-loop (cdr lis)))   ] ))))

                                        ;(define (every-pred . pred-lis) 
;  (lambda (x) (and ((car pred-lis) x ) ((cadr pred-lis) x ) )  ))


;; 全て述語がtrueであればtrueを返す手続きを返す
(define (every-pred . pred-lis) 
  (lambda (x) 
    (let every-pred-loop ((preds pred-lis))
      (cond [(null? preds) #t]
            [((car preds) x ) (every-pred-loop (cdr preds))]
            [else #f] ))))

;; いずれかの述語がtrueであればtrueを返す手続きを返す
(define (any-pred . pred-lis) 
  (lambda (x) 
    (let any-pred-loop ((preds pred-lis))
      (cond [(null? preds) #f]
            [((car preds) x ) #t]
            [else (any-pred-loop (cdr preds)) ]))))
      
;; 正の整数であればtrueを返す 
(define positive-integer? (every-pred integer? positive? ) )
;; 正の整数or0であればtrueを返す 
(define positive-or-zero? (any-pred  positive?  (lambda(x) (= x 0) ) ) )

;(print (positive-integer? 0) )
;(print (positive-integer? 4) )
;(print (positive-integer? 4.5) )
;(print (positive-integer? -2) )

;(print (positive-or-zero? 0) )
;(print (positive-or-zero? 3) )
;(print (positive-or-zero? -2) )

;;(let ((p '(1  2 2 3  4 5) ) )
 ;; (print  (delete 10 p equal?) ) ) 
                      
              