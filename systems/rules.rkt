#lang racket
(require "../engine.rkt"
         "../first-order.rkt"
         )
(provide logic-rockets)

(define (get-rockets state components)
  (define rules-fn (get-rules state))
  (define position (get-component components 'position))
  (define rotation (get-component components 'rotation))
  (define pos-x (first position))
  (define pos-y (second position))
  
  ;(define nlb (< pos-x 100))
  ;(define nrb (> pos-x 700))
  ;(define facing-left (= rotation 90))
  ;(define facing-right (= rotation 270))

  (define rules (rules-fn pos-x pos-y rotation))
  
  (define vars (first (apply evaluate-assertions rules)))

  (list
   (dict-ref vars 'bow-port-rocket #f)
   (dict-ref vars 'bow-starboard-rocket #f)
   (dict-ref vars 'stern-port-rocket #f)
   (dict-ref vars 'stern-starboard-rocket #f)
   (dict-ref vars 'booster-rocket #f))
  )

(define logic-rockets
  (system '(rockets)
    (lambda (state id components)
      (define next-rockets (get-rockets state components))
      (hash-set components 'rockets next-rockets)
      )))
