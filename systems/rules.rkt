#lang racket
(require "../engine.rkt"
         "../rules.rkt")
(provide logic-rockets)

(define (get-rockets current-rockets state components)
  (run-logic current-rockets state components))

(define logic-rockets
  (system '(rockets)
    (lambda (state id components)
      (define rockets (get-component components 'rockets))
      (define next-rockets (get-rockets rockets state components))
      (hash-set components 'rockets next-rockets)
      )))