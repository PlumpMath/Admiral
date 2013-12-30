(ns admiral.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! chan put!]]
            [admiral.render.core :as draw]
            [admiral.physics :as physics]))

(def pi (.-PI js/Math))

;; Gamestate
;; The world is 1000 x 500
(def test-gamestate
  {:entities {:blue-1 {:id :blue-1
                       :model :ship
                       :faction :blue
                       :rotation 0
                       :pos [100 300]}
              :blue-2 {:id :blue-2
                       :model :ship
                       :faction :blue
                       :rotation pi
                       :pos [200 200]}
              :red-1 {:id :red-1
                      :model :ship
                      :faction :red
                      :rotation (* pi 1.5)
                      :pos [400 400]}}})

(def callback nil)
(defn render-tick
  "Returns a channel that gets a tick value placed 60x / second"
  []
  (let [tick (chan)]
    (set! callback (fn [t]
                     ((or js/requestAnimationFrame
                          js/webkitRequestAnimationFrame)
                      callback)
                     (put! tick t)))
    (callback 0.0)
    tick))

(defn main []
  (let [gamestate (atom test-gamestate)
        renderer (draw/create-canvas-renderer "canvas")
        engine (physics/create-physics-engine)
        tick (render-tick)]

    ;; Main Loop
    (go
     (loop [t (<! tick)]
       (when t
         ;;(.log js/console t)
         ;; physics tick
         (swap! gamestate #(physics/step engine % t))
         ;; render
         (draw/render renderer @gamestate)
         (recur (<! tick)))))
    ))

(aset js/window "onload" main)
