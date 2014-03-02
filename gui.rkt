#lang racket/gui
(require "render.rkt" "engine.rkt" "rules.rkt")

(define FRAME-WIDTH 1200)
(define FRAME-HEIGHT 600)
(define CANVAS-WIDTH 800)
(define CANVAS-HEIGHT 600)
(define EDITOR-WIDTH 400)
(define EDITOR-HEIGHT 600)

(define TICKS-PER-SECOND 60)

(define frame (new frame% [label "Admiral"]
                   [width FRAME-WIDTH]
                   [height FRAME-HEIGHT]))

(define msg (new message% [parent frame]
                          [label "No events so far..."]))

(define panel (new horizontal-panel% [parent frame]
                                     ))

(define editor (new editor-canvas% [parent panel]
                                   [min-width EDITOR-WIDTH]
                    ))
(define game (new canvas% [parent panel]
                  [min-width CANVAS-WIDTH]))

(send frame show #t)