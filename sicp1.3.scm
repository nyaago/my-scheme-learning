; sicp 1.3

; a から bまで値をnext手続きで増分していき、
; それらの値にterm手続きで指定した演算を適用した値を合計する手続き.
; term, nextとも１つの引数を要求する手続き
(define (sum term a next b)
  (if (> a b) 
    0
    (+ (term a) 
       (sum term (next a) next b) ) )
)


(define (sum-integers a b)
  (define (inc x)  (+ x 1) )
  (define (identity x) x)
  (sum identity a inc b)
)

; Exercise 1.30
; 末尾再帰
; 増分値といままで加算してきた値を引数に渡しながら反復する.
(define (sum term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter  (next a) (+ result (term a) ) ) ) )
  (iter a 0) )
 

; Exercise 1.32
; a から bまで値をnext手続きで増分していき、
; それらの値にterm手続きで指定した演算を適用した値をcombinerで指定した手続きで積み上げる手続き.
; term, nextとも１つの引数を要求する手続き

; 非末尾再帰
(define (accumulate combiner null-value term a next b)
  (if (> a  b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b )))
)

; 末尾再帰
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
  (if (> a  b)
      result
      (iter (next a) 
            (combiner result (term a) ) ) ) )
  (iter a null-value)
)
 


; 合計値をとる
(define  (sum term a next b)
  (accumulate + 0 term a next b) )

; 積をとる場合
(define  (product term a next b)
  (accumulate * 1 term a next b) )

; 
(define (sum-integers a b)
  (define (inc x)  (+ x 1) )
  (define (identity x) x)
  (sum identity a inc b)
)

; 
(define (product-integers a b)
  (define (inc x)  (+ x 1) )
  (define (identity x) x)
  (product identity a inc b)
)


; 1.3.3
; 関数の入力と出力の差がなくなる時点の入力値を
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y) ) tolerance) ) 
  (define (try guess)
    (let ((next (f guess) ) )
       (if (close-enough? next guess)
           next
           (try  next) ) ) ) 
  (try first-guess) )

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)          
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y) ) ) 1.0) )

; 1.3.4
; 指定した手続きの入力と出力の平均を求める手続きを返す手続き
(define (average-dump f)
  (lambda (x) (average x (f x))))

; その手続きを手続きの差が十分小さくなるときの出力値を求める手続き(fixed-point)にわたして
; 平方根をとく
; 初期値1.0とした算出対象の元の値を割る値（入力）と割られた値（出力）の差が十分小さくなりまで
; 割り算を再帰的に行う。
; 十分値が小さくなったときの割り算結果を平方根の解とする。
; ここでは、以下の３つの手続きを組み合わせていることになる。
; -1.x/yを求めるlambda
; -2.指定した手続きの入力と出力の平均を求める手続きを返す手続き(average-dump)
; -3.手続きの差が十分小さくなるときの出力値を求める手続き(fixed-point)

(define (sqrt x)
  (fixed-point (average-dump (lambda (y) (/ x y) ) ) 1.0) )


; さらに、上記の2の手続き(agerage-dump)から1つめの手続き（x/yを求めるlambda)を呼び出すのではなく、
; それぞれの手続き引数で渡すように抽象化すると。

(define (fixed-point-of-transform g transform guess)
  (fixed-point  (transform g) guess) )

(define (sqrt x)
  (fixed-point-of-transform (lambda (y)  (/ x y) )
                            average-dump
                            1.0 ) )

; 
(define (iter-proc f x n)
  (if (= n 0)
      x)
  (iter-proc (f f) (- n 1) ) 
)

; exercise 1.41
(define (iter-proc  proc arg-for-proc  n)
  (cond ( (= n 0) arg-for-proc)
        (else (iter-proc proc  (proc arg-for-proc) (- n 1)   )  ) ) )



(define (double f)
  (lambda (x) (iter-proc f x  2)) )

(define (triple f)
  (lambda (x) (iter-proc f x  3)) )


(define (double f)
  (lambda (x)
     (f (f x))
     ))
   
; exercise 1.42
(define (compose  f g)
  (lambda (x)
    (f (g x) ) )
)
                                      