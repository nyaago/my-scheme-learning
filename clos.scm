#;(define-class <book> (<object>)
  ((title :init-keyword :title)
   (price :init-keyword :price)
   (opened-at :init-keyword :opened-at)
   (auther :init-keyword :auther)
   (available :init-keyword :available :init-value #t)
) )



;(define book  (make <book>  :title "白痴" :auther "坂口安吾"  ) )
;(ref book 'auther)
;(ref book 'title)


#;(define-class <foo-meta> (<class>)
  ((number :init-value 10 )))

#;(define-class <foo> ()
  ()
 :meta-class <foo-meta>
)


;; ;; 総称関数に前後処理を適用

;; 総称関数クラス
(define-class <logger-generic> (<generic>)
  ( (enable-log :init-keyword :enable-log :init-value #t) 
  (enable-profile :init-keyword :enable-profile :init-value #t)
))


;; 総称関数
(define-generic add :class <logger-generic> )
(define-generic sub :class <logger-generic> )


(define-method apply-generic ((gf <logger-generic>) args)
  (when (ref gf 'enable-log) 
   (format #t "args: ~s\n" args) )
  (let ((return-value (next-method)))
    (when (ref gf 'enable-log) 
          (format #t "result: ~s\n" return-value) )
    return-value)
)

(define-method add ((num1 <number>) (num2 <number> ))
  (+ num1 num2)
)

(define-method sub ((num1 <number>) (num2 <number> ))
  (- num1 num2)
)



;(define-generic ebable-log :class <generic> )
(define-method enable-log ((gf <logger-generic>) bool)
  (set! (ref gf  'enable-log) bool)
)
;(set!  (ref add 'enable-log) #f )
(enable-log add #t)
((add 3 4))



;(d (make <book> :title "白痴" :auther: "坂口安吾") )