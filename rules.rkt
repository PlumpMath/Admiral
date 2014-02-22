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

;; todo, better way to pull these vars.
;; returns the rockets.
(define (get-rockets pos-x rotation rules)

  ;; sorta copying this from the boolean logic interpretor here
  ;; https://github.com/webyrd/meta-interp/blob/master/boolean.scm
  ;; I think I need a similar approach.

  ;; defines all the possible forms
  (define termo
    (lambda (t)
      (conde
       [(fresh (t1 t2)
               (== `(UNIFY ,t1 ,t2) t)
               (termo t1)
               (termo t2))]
       [(fresh (t1 t2)
               (== `(AND ,t1 ,t2) t)
               (termo t1)
               (termo t2))]
       [(fresh (t1 t2)
               (== `(OR ,t1 ,t2) t)
               (termo t1)
               (termo t2))]
       [(fresh (t1)
               (== `(NOT ,t1) t)
               (termo t1))]
       [(== #t t)]
       [(== #f t)])))

  (define ruleseto
    (lambda (ts)
      (conde
       [(fresh (f r)
               (== (cons f r) ts)
               (termo f)
               (ruleseto r))])))

  ;; oh! I think I might need the concept of an environment which is
  ;; where the vars that I actually want to unify would be.

  (define not-in-envo
    (lambda (x env)
      (conde
       [(fresh (k v rest)
          (== `((,k . ,v) . ,rest) env)
          (=/= k x)
          (not-in-envo x rest))]
       [(== '() env)])))

  (define get-from-envo
    (lambda (x env val)
      (fresh (k v rest)
        (== `((,k . ,v) . ,rest) env)
        (conde
         [(== k x)
          (== v val)]
         [(get-from-envo x rest val)]))))
  
  (define evalo
    (lambda (exp c-env n-env val)
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
          ;; Not sure about this because there's no real result.
          (== val v2))]
       ;; Think I need this maybe for literal true and false if that's
       ;; a thing?
       [(== exp #t)
        (== c-env n-env)
        (== exp val)]
       [(== exp #f)
        (== c-env n-env)
        (== exp val)])))

  ;;(run 1 (q) (fresh (v) (eval-all `((UNIFY foo bar) (UNIFY bar #t)) `() q)))
  (define eval-all
    (lambda (exps c-vars n-vars)
      (conde
       [(fresh (f r env* v)
               (== (cons f r) exps)
               (evalo f c-vars env* v)
               (eval-all r env* n-vars))]
       [(== `() exps)
        (== c-vars n-vars)])))

  ;; Needed for finally resolving shit to a bool instead of an
  ;; expression that resolves to a bool.
  ;; if you run evalo and for instance unify something to false that
  ;; something actually resolves to any expression that resolves to
  ;; false so you have to restrict it to being a bool if you want one
  ;; answer.
  ;; mindblown.gif
  (define boolo
    (lambda (b)
      (conde
       [(== b #t)]
       [(== b #f)])))

  ;; This is working really well and returns #f
  ;;  (run 1 (q) (fresh (result blah) (evalo `(UNIFY hello ,q)
  ;;  `((hello . #f)) blah result)) (boolo q))

  ;; If I run it with run* though it runs forever, need to figure out
  ;; a way to get it to not do that or something, or maybe it's fine?
  ;; well, there is only one solution so I don't want it to keep
  ;; running

  `(#f #f #f #t #f)
  )

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
  (get-rockets pos-x rotation rules))

(define (run-logic current-rockets state components)
  (get-rockets-by-mk-interpretor current-rockets state components)
  )
