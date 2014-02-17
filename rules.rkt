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
;; I don't know how to write macros yet so that's a bummer. Gonna just
;; hack at this bitch a bit and see what comes out.



(define (get-rockets-by-dumb-rules current-rockets state components)
  ;; First the real shitty version!
  (define position (get-component components 'position))
  (define rotation (get-component components 'rotation))
  (define pos-x (first position))
  (define boost `(#f #f #f #f #t))
  (define rotate `(#f #t #t #f #f))
  (cond
   [(< pos-x 100) (if (= rotation 270)
                     boost
                     rotate)]
   [(> pos-x 900) (if (= rotation 90)
                      boost
                      rotate)]
   [else boost]))

(define (get-rockets-by-naive-interpretor current-rockets state components)
  ;;
  (define position (get-component components 'position))
  (define rotation (get-component components 'rotation))
  (define pos-x (first position))

    (define boost `(#f #f #f #f #t))
    (define rotate `(#f #t #t #f #f))

    rotate
  )

(define (run-logic current-rockets state components)
  (rockets-from-mk current-rockets state components)
  ;(get-rockets-by-dumb-rules current-rockets state components)
  )
