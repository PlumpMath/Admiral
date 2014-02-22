#lang racket
(require "miniKanren/mk.rkt")
(provide eval-rules)

;; succeeds if key is not in environment env.
(define (not-in-envo key env)
  (conde
   [(fresh (k v rest)
      (== `((,k . ,v) . ,rest) env)
      (=/= k key)
      (not-in-envo key rest))]
   [(== '() env)]))

;; unifies val with the value associated to key in environment env.
(define (get-from-envo key env val)
  (fresh (k v rest)
    (== `((,k . ,v) . ,rest) env)
    (conde
     [(== k key)
      (== v val)]
     [(get-from-envo key rest val)])))

;; unifies r with (and a b)
(define (ando a b r)
  (conde
   [(== a #t) (== b #t) (== r #t)]
   [(== a #f) (== b #t) (== r #f)]
   [(== a #t) (== b #f) (== r #f)]
   [(== a #f) (== b #f) (== r #f)]))

;; unifies r with (or a b)
(define (oro a b r)
  (conde
   [(== a #t) (== b #t) (== r #t)]
   [(== a #f) (== b #t) (== r #t)]
   [(== a #t) (== b #f) (== r #t)]
   [(== a #f) (== b #f) (== r #f)]))

;; unifies v with (not x)
(define (noto x v)
  (conde
   [(== x #t) (== v #f)]
   [(== x #f) (== v #t)]))

;; succeeds if b is a boolean value
(define (boolo b)
  (conde
   [(== b #t)]
   [(== b #f)]))

(define (evalo exp c-env n-env val)
  (conde
   ;; New user defined variable.
   [(symbolo exp)
    (not-in-envo exp c-env)
    (fresh (v)
      (== val v)
      (== n-env `((,exp . ,v) . ,c-env)))]
   ;; Existing variable
   [(symbolo exp)
    (get-from-envo exp c-env val)
    (== c-env n-env)]
   ;; Unify two expressions
   [(fresh (e1 e2 v1 v2 env*)
      (== `(UNIFY ,e1 ,e2) exp)
      (evalo e1 c-env env* v1)
      (evalo e2 env* n-env v2)
      (== v1 v2)
      ;; Not sure about this because there's no real result of a unification.
      (== val v2))]
   ;; And
   [(fresh (e1 e2 v1 v2 env*)
      (== `(AND ,e1 ,e2) exp)
      (evalo e1 c-env env* v1)
      (evalo e2 env* n-env v2)
      (ando v1 v2 val))]
   ;; OR
   [(fresh (e1 e2 v1 v2 env*)
      (== `(OR ,e1 ,e2) exp)
      (evalo e1 c-env env* v1)
      (evalo e2 env* n-env v2)
      (oro v1 v2 val))]
   ;; Not
   [(fresh (e v)
      (== `(NOT ,e) exp)
      (evalo e c-env n-env v)
      (noto v val))]
   ;; Hmm
   [(boolo exp)
    (== c-env n-env)
    (== exp val)]))

(define (eval-manyo exps c-vars n-vars)
  (conde
   [(fresh (f r env* v)
      (== (cons f r) exps)
      (evalo f c-vars env* v)
      (eval-manyo r env* n-vars))]
   [(== `() exps)
    (== c-vars n-vars)]))

;; Takes the ruleset datastructure and the predefined variables
;; and returns all defined vars.
(define (eval-rules rule-set input-vars)
  (define output-vars-seq
    (run 1 (q) (eval-manyo rule-set input-vars q)))
  (first output-vars-seq))

;; Tests
(module+ test-orders (require rackunit)
  (run* (q) (eval-manyo
             `((UNIFY foo #t)
               (UNIFY bar #f)
               (UNIFY (AND foo bar) baz))))
  
  ;; (define nlb #f)
  ;; (define nrb #f)
  ;; (define facing-left #f)
  ;; (define facing-right #t)
  ;; (define rules `((UNIFY ,nlb near-left-border)
  ;;                 (UNIFY ,nrb near-right-border)
  ;;                 (UNIFY rotate-clockwise (AND bow-starboard-rocket
  ;;                                              stern-port-rocket
  ;;                                              (NOT booster-rocket)))
  ;;                 (UNIFY (AND near-left-border (NOT ,facing-left))
  ;;                        (AND rotate-clockwise avoiding-right-wall))
  ;;                 (UNIFY (AND near-right-border (NOT ,facing-right))
  ;;                        (AND rotate-clockwise avoiding-left-wall))
  ;;                 (UNIFY (AND (NOT avoiding-right-wall)
  ;;                             (NOT avoiding-left-wall))
  ;;                        booster-rocket)))
  )
