#lang racket/gui
#| This whole file is very "hack it till it works" right now. TODO come back
   and rethink just about everything
|#

;; TODO: Need the editor and the engine to use different event spaces so that
;; typing is responsive. Also play with frames / second for simulation because
;; it's real laggy.
(require "config.rkt"
         "render.rkt"
         "update.rkt"
         "engine.rkt"
         (only-in mrlib/image-core render-image))

(define FRAME-WIDTH 1200)
(define FRAME-HEIGHT 600)
(define EDITOR-WIDTH 400)
(define EDITOR-HEIGHT 600)
;; world and canvas size
;;(define CANVAS-WIDTH 800)
;;(define CANVAS-HEIGHT 600)

;;(define TICKS-PER-SECOND 60)

(define base-rules
 "(lambda (pos-x pos-y rotation)
    `((IFF ,(< pos-x 100) near-left-border)
      (IFF ,(> pos-x 700) near-right-border)
      (IFF ,(= rotation 90) facing-left)
      (IFF ,(= rotation 270) facing-right)
      (IFF rotate-clockwise bow-starboard-rocket)
      (IFF rotate-clockwise stern-port-rocket)
      
      (IF (AND near-left-border (NOT facing-right))
          rotate-clockwise)
      (IF (AND near-right-border (NOT facing-left))
          rotate-clockwise)
    
      (IF (AND (NOT near-right-border)
               (NOT near-left-border))
          booster-rocket)
    
      (IF (AND near-left-border facing-right)
          booster-rocket)
    
      (IF (AND near-right-border facing-left)
          booster-rocket)
    
      (IFF (NOT rotate-clockwise) booster-rocket)
      ))")

;; mutable bullshit for now
(define world a-world)

(define (tick!)
  (set! world (update world))
  (send game refresh))

;; the game
;;(define update (get-update CANVAS-WIDTH CANVAS-HEIGHT))

;; Returns the function that takes the gamestate and makes the image
;;(define renderor (get-render-function CANVAS-WIDTH CANVAS-HEIGHT))

;; Used as the paint callback for the canvas
(define (paint! canvas context)
  (define image (render world))
  (render-image image context 0 0))

;; Main window
(define frame (new
               (class frame%
                 (super-new)
                 ;; Do I need this?
                 ;;(define/augment (on-close)
                 ;;          (custodian-shutdown-all (current-custodian)))
                 )
               [label "Admiral"]
               [width FRAME-WIDTH]
               [height FRAME-HEIGHT]))

;; Namespace for evaluation of rocket language lambda.
;; needs to have `lambda` in it, might be able to actuially implement
;; rocket language as a racket #lang and then use that for the ns and
;; eval the language directly!
(define ns (make-base-namespace))

(define (button-callback b e)
  (define rule-string (send t get-text))
  (define r (read (open-input-string rule-string)))
  (define l (eval r ns))
  (set! world (set-rules world l)))

(define button (new button%
                    [parent frame]
                    [label "Eval Rules"]
                    [callback button-callback]))

(define panel (new horizontal-panel%
                   [parent frame]
                   [style `(border)]))

(define editor (new editor-canvas%
                    [parent panel]
                    [min-width EDITOR-WIDTH]
                    ))

(define t (new text%))
(send t insert base-rules 0)
(send editor set-editor t)

(send editor set-editor t)

(define game (new (class canvas%
                    (super-new)
                    
                    ;;(define/override (on-char key-event)
                    ;;  (when (eq? (send key-event get-key-code) 'release)
                    ;;    (set! state 0)
                    ;;    (send game refresh))))
                    
                    )
                  [parent panel]
                  [min-width (CANVAS-WIDTH)]
                  [paint-callback paint!]))

(send frame show #t)

(define timer (new timer%
                   [notify-callback tick!]
                   [interval 20]))
