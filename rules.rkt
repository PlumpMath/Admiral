#lang racket
(require "miniKanren/mk.rkt")

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


(define (get-rockets-by-naive-interpretor current-rockets state components)
  ;;
  (define position (get-component components 'position))
  (define rotation (get-component components 'rotation))
  (define pos-x (first position))

  (define boost `(#f #f #f #f #t))
  (define rotate `(#f #t #t #f #f))
  
  (struct orders (queries user-actions rules) #:transparent)
  (struct rule (queries negated-queries actions) #:transparent)

  ;; what I might want the parsed rules to look like.
  (define rules (orders
                 `#hash((near-left-border . ,(< pos-x 100))
                        (near-right-border . ,(> pos-x 900))
                        (facing-right . ,(= rotation 270))
                        (facing-left . ,(= rotation 90)))
                 `#hash((rotate-clockwise . ((fire-rocket bs)
                                             (fire-rocket sp))))
                 ;; todo, dependencies stuff
                 `#hash((avoiding-left . ,(rule `(near-left-border)
                                                `(facing-right)
                                                `(rotate-clockwise)))
                        (avoiding-right . ,(rule `(near-right-border)
                                                 `(facing-left)
                                                 `(rotate-clockwise)))
                        (otherwise . ,(rule `()
                                            `(avoiding-left avoiding-right)
                                            `(fire-rocket boost))))))
  boost)


(define (get-rockets-by-mk-interpretor current-rockets state components)
  (define position (get-component components 'position))
  (define rotation (get-component components 'rotation))
  (define pos-x (first position))

  ;;gonna start with a non relational interpreter and hopefully build a
  ;;relational one with minikanren later on.
  (define LT (lambda (x y) (< x y)))
  (define GT (lambda (x y) (> x y)))
  (define AND (lambda (x y) (and x y)))
  (define OR (lambda (x y) (or x y)))
  (define NOT (lambda (x) (not x)))
  (define IMPLIES (lambda (premise conclusion) (if premise conclusion #t)))

  ;; this is what the language / gui parses into.
  (define rules `((UNIFY (LT pos-x 100) near-left-border)
                  (UNIFY (GT pos-x 900) near-right-border)
                  (UNIFY rotate-clockwise (AND bow-starboard-rocket
                                               stern-port-rocket))
                  (UNIFY (AND near-left-border (NOT facing-left))
                         (AND rotate-clockwise avoiding-right-wall))
                  (UNIFY (AND near-right-border (NOT facing-right))
                         (AND rotate-clockwise avoiding-left-wall))
                  (UNIFY (AND (NOT avoiding-right-wall)
                              (NOT avoiding-left-wall))
                         booster-rocket))
    )

  ;; I think I need minikanren for this... Maybe I can actually write
  ;;this! o boy that would just be the best thing ever!
  
  


  )



(define (run-logic current-rockets state components)
  (get-rockets-by-interpretor current-rockets state components)
  ;(get-rockets-by-dumb-rules current-rockets state components)
  )
