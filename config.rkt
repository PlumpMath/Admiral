#lang racket/base
;; Config values used by admiral. Default values for testing here but may be parameterized.
(provide WORLD-WIDTH
         WORLD-HEIGHT
         CANVAS-WIDTH
         CANVAS-HEIGHT
         TICKS-PER-SECOND)

;; The width and height of the simulated world.
(define WORLD-WIDTH (make-parameter 800))
(define WORLD-HEIGHT (make-parameter 600))

;; The width and height of the drawing canvas, used for scaling.
(define CANVAS-WIDTH (make-parameter 800))
(define CANVAS-HEIGHT (make-parameter 600))

(define TICKS-PER-SECOND (make-parameter 60))