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

;; Built in
;;   actions
;;     fire-rocket
;;   queries
;;     pos-x
;;     pos-y
;;     rotation
;;   operators? is this even what these are called?
;;     ==, <, <=, >, >=, !=
;;     seems like integer arithmatic is needed so maybe can't use
;;     minikanren or maybe still can or I dunno.

;; ;; User rules
;; (define rules
;;   (list
   ;; (def-action (rotate-clockwise)
   ;;   (fire-rocket bs)
   ;;   (fire-rocket sp))

   ;; (def-query (near-left-boarder)
   ;;   (< pos-x 100))

   ;; (def-query (near-right-boarder)
   ;;   (> pos-x 900))

   ;; (def-query (facing-right)
   ;;   (= rotation 270))

   ;; (def-query (facing-left)
   ;;   (= rotation 90))

   ;; (def-rule avoiding-left
   ;;   (near-left-boarder)
   ;;   (not (facing-right))
   ;;   =>
   ;;   (rotate-clockwise))

   ;; (def-rule avoiding-right
   ;;   (near-right-boarder)
   ;;   (not (facing-left))
   ;;   =>
   ;;   (rotate-clockwise))

   ;; (def-rule
   ;;   (not (avoiding-left))
   ;;   (not (avoiding-right))
   ;;   =>
   ;;   (fire-rocket boost))))

;; What I need the rules to look like, post macro. First guess?
(define rules
  ;; stuff will have to be defined from the gamestate.
  (define pos-x 99)
  (define rotation 90)
  (define rotate-clockwise
    (lambda (bp bs sp ss boost)
      (fresh ()
        (== bs #t)
        (== sp #t))))
  (run* (q)
        (fresh (near-left-border near-right-border
                facing-right facing-left
                avoiding-left avoiding-right
                ;; rotate-clockwise
                bp bs sp ss boost
                
                )
          (== near-left-border (< pos-x 100))
          (== near-right-border (> pos-x 900))
          (== facing-right (= rotation 270))
          (== facing-left (= rotation 90))
          (conde
           [(== avoiding-left #t) ;; maybe just succeed?
            (== near-left-border #t)
            (== facing-right #f)
            (rotate-clockwise bp bs sp ss boost)] ;; hmm TODO
           [(== avoiding-right #t)
            (== near-right-border #t)
            (== facing-left #f)
            (rotate-clockwise bp bs sp ss boost)] ;; see above
           [(== avoiding-left #f)
            (== avoiding-right #f)
            (== boost #t)]
           )
          (== q `(,near-left-border ,near-right-border
                                    ,facing-right
                                    ,facing-left
                                    ,avoiding-left
                                    ,avoiding-right
                                    ,bp ,bs ,sp ,ss ,boost)))
  )
;;

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

(define (run-logic current-rockets state components)
  (get-rockets-by-dumb-rules current-rockets state components)
  )
