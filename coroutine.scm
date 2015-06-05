(use util.queue)

(define *tasks* (make-queue) )


(define-syntax define-coroutine
  (syntax-rules () 
    [ (_  (routine yield) body ...)
      (define (routine)
        (call/cc ( lambda(return) 
                   (define (yield)
                     (call/cc (lambda (cont)
                              (enqueue! *tasks* cont)
                              (return)) ))
                   body ...))
        ((dequeue! *tasks*))) ] ) )


(define-syntax define-coroutine
  (syntax-rules () 
    [ (_  (routine yield) body ...)
      (define (routine)
        (call/cc ( lambda(return) 
                   (define (yield)
                     (call/cc (lambda (cont)
                              (enqueue! *tasks* cont)
                              (return)) ))
                   body ...))
        ((dequeue! *tasks*))) ] 
    [ (_ (routine yield exit) body ...)
      (define (routine)
        (call/cc ( lambda (escape) 
                   (call/cc ( lambda(return) 
                              (define (yield)
                                (call/cc (lambda (cont)
                                           (enqueue! *tasks* cont)
                                           (return)) ))
                   (define (exit) 
                     (call/cc (lambda (cont) 
                                ( (enqueue! *tasks* cont)
                                 (escape) ) ) ) )
                   body ...))
                   ((dequeue! *tasks*) ) ) ) )
     ]))



(define (coroutine-init! . rs)
  (set! *tasks* (make-queue))
  (for-each (lambda (r)
              (enqueue! *tasks* r) )
            rs))
(define (coroutine-add! r)  (enqueue! *tasks r))
(define (coroutine-del! r)  (dequeue! *tasks ) )
(define (coroutine-restart! r) ( (dequeue! *tasks )  ) )
(define (coroutine-skip!) (coroutine-add! (coroutine-del!)  ) )




(define-coroutine (three yield)
  (let lp ()
    (print 'one)
    (yield)
    (print 'two)
    (yield)
    (print 'tree)
    (yield)
    (lp)))


(use gauche.time)
(define *timer* #f)
(define (start-timer)
  (set! *timer* (make <real-time-counter> ) )
  (time-counter-start! *timer*)
)

(define-coroutine (exit-by-counter yield exit-co)
  (let lp ()
    (time-counter-stop! *timer*)
;    (print (time-counter-value *timer*) )
  (if (> (time-counter-value *timer*) 10)
      (exit-co)
      )
  (time-counter-start! *timer*)
  (yield)
  (lp)) )


(coroutine-init! exit-by-counter )

(define-coroutine (expt-co yield)
  (let lp ((v 2))
    (if (> v 1000) (exit-co))
    (print  v)
    (set! v (expt v 2) )
    (yield)
    (lp v)))


(define-coroutine (two yield)
  (let lp ()
    (print 'one)
    (yield)
    (print 'two)
    (yield)
    (lp)))



;(three)

(define-coroutine (a yield )
  (print 'a) )

(define-coroutine (abc yield)
  (let lp ()
    (print 'a)
    (yield)
    (print 'b)
    (yield)
    (print 'c)
    (yield)
    (lp)))


(coroutine-init! a)

(three)