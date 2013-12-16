(ns admiral.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! chan put!]]
            [admiral.render.core :as draw]))

(def pi (.-PI js/Math))

;; Gamestate
(def test-gamestate
  {:entities {:blue-1 {:id :blue-1
                       :model :ship
                       :faction :blue
                       :rotation 0
                       :pos [200 200]}
              :blue-2 {:id :blue-2
                       :model :ship
                       :faction :blue
                       :rotation pi
                       :pos [200 200]}
              :red-1 {:id :red-1
                      :model :ship
                      :faction :red
                      :rotation (* pi 1.5)
                      :pos [200 200]}}})

(def request-animation-frame
)

(def callback nil)
(defn render-tick
  "Returns a channel that gets a tick value placed 60x / second"
  []
  (let [tick (chan)]
    (set! callback (fn []
                     ((or js/requestAnimationFrame
                          js/webkitRequestAnimationFrame)
                      callback)
                     (put! tick :render)))
    (callback)
    tick))

(defn main []
  (let [gamestate (atom test-gamestate)
        renderer (draw/create-canvas-renderer "canvas")
        tick (render-tick)]

    ;; Render Loop
    (go
     (loop [t (<! tick)]
       (when t
         (draw/render renderer @gamestate)
         (recur (<! tick)))))


    ))

(aset js/window "onload" main)
