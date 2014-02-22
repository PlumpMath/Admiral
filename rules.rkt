#lang racket
(require "orders.rkt")

;; I have not figured this out at all yet. Need to figure out good
;; ways to write the ai.
(provide run-logic)

;; OMG THIS DONT GO HERE OH LORDY TODO
;; needs to be game engine code file.
(define (has-component? entity component-type)
  (hash-has-key? entity component-type))

(define (get-component entity component-type)
  (hash-ref entity component-type #f))
;; ------------------------------

;; todo, better way to pull these vars.
;; returns the rockets.
(define (get-rockets pos-x rotation rules)


  `(#f #f #t #t #f)
  )

(define (get-rockets-by-mk-interpretor current-rockets state components)
  (define position (get-component components 'position))
  (define rotation (get-component components 'rotation))
  (define pos-x (first position))

  ;; TODO later, the actual math in the rules
  (define nlb (< pos-x 100))
  (define nrb (> pos-x 900))
  (define facing-left (= rotation 90))
  (define facing-right (= rotation 270))


  (define rules `((UNIFY ,nlb near-left-border)
                  (UNIFY ,nrb near-right-border)
                  (UNIFY rotate-clockwise (AND bow-starboard-rocket
                                               stern-port-rocket))
                  (UNIFY (AND near-left-border (NOT ,facing-left))
                         (AND rotate-clockwise avoiding-right-wall))
                  (UNIFY (AND near-right-border (NOT ,facing-right))
                         (AND rotate-clockwise avoiding-left-wall))
                  (UNIFY (AND (NOT avoiding-right-wall)
                              (NOT avoiding-left-wall))
                         booster-rocket)))

  (define vars (eval-rules rules `()))
  (display "vars\n")
  (display vars)
  (display "\n")

  (list
   (dict-ref vars 'bow-port-rocket #f)
   (dict-ref vars 'bow-starboard-rocket #f)
   (dict-ref vars 'stern-port-rocket #f)
   (dict-ref vars 'stern-starboard-rocket #f)
   (dict-ref vars 'booster-rocket #f))

  ;(get-rockets pos-x rotation rules)
  )

(define (run-logic current-rockets state components)
  (get-rockets-by-mk-interpretor current-rockets state components)
  )
