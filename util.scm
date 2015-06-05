(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define ++
  (lambda (x)
    (+ x 1)))

(define --
  (lambda (x)
    (- x 1)))


(define **
  (lambda (a b)
    (expt a b)))

; 商
(define //
  (lambda (a b)
    (quotient a b)))

; 商の余り
(define /@
  (lambda (a b)
    (remainder a b)))

;
(define /:
  (lambda (a b)
    (modulo a b)))


; 数値(x) を指定小数桁数(decimals)で丸める
(define adjust-of
  (lambda (x decimals)
    (let ((digit (expt 10 decimals))
          (slide (if (positive? x) 1/2 -1/2)))
      (/ (truncate (+ slide (* digit x))) digit))))
    

; min から max までの整数リスト を返す
(define itoa
  (lambda (min max)
    (if (> min max)
        '()
        (cons min (itoa (++ min) max)))))
;
(define itoa
  (lambda (min max)
    (let loop ((i max) (tmp '()))
      (if (< i min)
          tmp
          (loop (-- i) (cons i tmp))))))
               


; min から max までの整数リスト（逆順） を返す
(define itoa-reverse
  (lambda (min max)
    (if (> min max)
        '()
        (cons max (itoa-reverse min (-- max))))))


; itoa [min] max [step]
; min から max step 区切りのリスト を返す
; min は省略すると 1を採用
; step は省略すると 1を採用
(define itoa
  (lambda lst
    (let* ((x (length lst))
           (max (if (= 1 x) (car lst) (cadr lst)))
           (min (if (< 1 x) (car lst) 1))
           (step (if (<= 3 x)  (caddr lst) 1)))
      (let loop ((i min) (tmp '()))
        (if (< max i)
            (reverse tmp)
            (loop (+ i step)
                  (cons (adjust-of i 3) tmp)))))))


; 階乗
(define fact
  (lambda (n)
    (if (zero? n) 
        1
        (* n (fact (- n 1))))))
; 階乗 (末尾再帰）
(define fact
  (lambda (n)
    (define fact-iter 
      (lambda (y p)
        (if (zero? y)
            p
            (fact-iter (- y 1) (* y p)))))
    (fact-iter n 1)))
; 階乗(名前付きlet）
(define fact-let
  (lambda (n)
    (let countdown ((y n) (p 1))
      (if (zero? y)
          p
          (countdown (- y 1) (* y p))))))


; リストから最後の要素をPairとして取り出す
(define last-pair
  (lambda (lst)
    (list (list-ref lst (- (length lst) 1)))))

; atomであるか?
(define atom?
  (lambda (e)
    (and (not (null? e) )
         (not (pair? e)))))

; pair? の 否定述語
(define nonpair?
  (lambda (e)
    (not (pair? e))))

; リスト内のatomの総数.
(define length-all
  (lambda (e)
    (cond 
     ((null? e) 0)
     ((atom? e) 1)
     (else
      (+ (length-all (car e))
         (length-all (cdr e)))))))
         
; リスト内のreverse - 要素を再帰して
(define reverse-all
  (lambda (e)
    (cond
     ((null? e) '())
     ((atom? e) e)
     (else
      (append (reverse-all (cdr e))
            (list (reverse-all (car e))))))))

; ネストしたリストをフラットに
(define flatten
  (lambda (lst)
    (cond
     ((null? lst) '())
     ((atom? lst) (list lst))
     (else
      (append (flatten (car lst) )
            (flatten (cdr lst)))))))

; 指定した条件のみを含む要素のリスト
(define filter 
  (lambda (predi lst)
    (cond ((null? lst) '())
          ((predi (car lst))
           (cons (car lst) (filter predi (cdr lst))))
          (else
           (filter predi  (cdr lst))))))

; 指定した条件を除いた要素のリスト
(define remove 
  (lambda (predi lst)
    (cond ((null? lst) '())
          ((predi (car lst))
           (remove predi (cdr lst)))
          (else
           (cons (car lst) (remove predi  (cdr lst)))))))
; 条件の述語化
(define target?
  (lambda (proc x)
    (lambda (y)
      (proc x y))))
           
                       

; リストをかえすproc を与えて、結果としてflat化したリストを返す版のmap 
(define flatmap
  (lambda (proc lst)
    (apply append 
           (map  proc lst ))))
;(flatmap 
; (lambda (i)
;   (map 
;    (lambda (j)
;      (list i j) ) '(1 2))) 
; '(a b))

; ある自然数とそれより小さい自然数のPair のリスト
(define double
  (lambda (n)
    (apply append
           (map (lambda (i) 
                  (map  (lambda (j)  (list i j))
                        (itoa 1 (- i 1))))
                (itoa 1 n)))))

(define sq
  (lambda (x)
    (* x x)))

(define sq+
  (lambda (x y)
    (+ (sq x)  (sq y))))

        

; リストから指定された値を除く
(define del-obj
  (lambda (lst obj)
    (call/cc 
     (lambda (k)
       (cond ((null? lst) '())
             ((equal? (car lst) obj) 
              (k (cdr lst)))
             (else
              (cons (car lst)  
                    (del-obj (cdr lst) obj))))))))
; 順列
(define (permutations lst)              
  (if (null? lst) 
      (list '())
      (apply 
       append
       (map (lambda (i)
              (map (lambda (j)
                     (cons i j))
                   (permutations (del-obj lst i))))
              lst))))
              
; 順列数
; n = 要素数、r = 選択される要素数
(define (perm  n r)
  (cond ((= r 0) 1)
        ((= r 1) n)
        (else
         (* n (perm (- n 1) (- r 1))))))
; 順列数-階乗を使って 
; n = 要素数、r = 選択される要素数
(define (perm n r)        
  (/ (fact n) (fact (- n r))))
           

; 組み合わせ数
; n = 要素数、r = 選択される要素数
(define (comb n r)
  (if (or (= r 0) (= n r)) 
      1
      (+ (comb (- n 1) (- r 1))
         (comb (- n 1) r))))
; 組み合わせ数 - 順列を使って
; n = 要素数、r = 選択される要素数
(define (comb n r)         
  (/ (perm n r) (perm r r)))
; 組み合わせ数 - 階乗を使って
; n = 要素数、r = 選択される要素数
(define (comb n r)                           
  (/ (fact n) (* (fact r) (fact (- n r)))))
      
; 初期値と最終値までの整数に手続きを適用したものを合計する
(define  sum
  (lambda (initial final body)
    (if (> initial final)
        0
        (+ (body initial)
           (sum (+ initial 1) final body)))))


; 後退級数
; 1 - 1/2 + 3/1 - 4/1 ....
;(define log2
;  (lambda (n)
;    (/ (** -1 (- n 1) ) n)))
;(* 1.0 (sum 1 1000 log2))

; 初期値と最終値までの整数に手続きを適用したもののかけ算（乗積）
(define  product
  (lambda (initial final body)
    (if (> initial final)
        1
        (* (body initial)
           (product (+ initial 1) final body)))))


; Accumulate
; op => accumulateの計算手続き
; ini => 初期値
; seqs => 計算本体のリスト
(define accumulate
  (lambda (op ini seqs)
    (if (null? seqs)
        ini
        (op (car seqs)
                 (accumulate op ini (cdr seqs))))))
; 初期値と最終値までの整数に手続きを適用したものを合計する         
(define sum
  (lambda (ini fin body)
    (accumulate + 0 (map body (itoa ini fin)))))
; 後退級数
; 1 - 1/2 + 3/1 - 4/1 ....
;(define log2
;  (lambda (n)
;    (/ (** -1 (- n 1) ) n)))
;(* 1.0 (sum 1 1000 log2))

; 初期値と最終値までの整数に手続きを適用したもののかけ算（乗積）
(define product
  (lambda (ini fin body)
    (acuumulate * 1 (map body (itoa ini fin)))))
               

; 2乗数か?
(define pow2?
  (lambda (x)
    (let ((y (inexact->exact (round (** x 1/2)))))
      (= x (** y 2)))))

(define digit->string
  (lambda (lst)
    (list->string
     (map integer->char (map (lambda (x) (+ x 48)) lst)))))

; シーザー暗号
(define caesar
  (lambda (str k)
    (define (e-engine lst k)
      (map (lambda (x)
             (integer->char (+ k (char->integer x)))) lst))
    (list->string (e-engine (string->list str) k))))
      
                      
; 

(define pi 3.141592653589793)
  

(define (fib n)
  (define (fib-iter s1 s2 i)
    (if (zero? i)
        s2
        (fib-iter (+ s1 s2) s1  (- i 1))))
  (fib-iter 1 0 n))



(define (memorize proc)
  (let ((table '()))
    (lambda x  (let ((stock (assoc x table)))
                stock
                (if stock
                    (cdr stock)
                    (let ((data (apply proc x )))
                      (set! table
                           (cons (cons x data) table)) data) )))))



(define fib-memo
  (memorize (lambda (n)
              n
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else (+ ((fib-memo) (- n 1))
                             ((fib-memo) (- n 2))) )))))
              
(define (abc  a b)
  (let ((c 4))
  (lambda (d) (+ #?=b #?=a #?=c  #?=d)) ))