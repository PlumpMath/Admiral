(ns admiral.physics)
;; Physics!
;; uses PhysicsJS
;; welcome to mutability junction

(defmulti create-body :model)

(defmethod create-body :ship [{:keys [pos rotation]}]
  (.body js/Physics )
  )

(defprotocol PhysicsEngine
  "A physics engine"
  (step [this gamestate time] "Advance the simulation."))

(deftype PhysicsJSEngine [world known-bodies]
  PhysicsEngine
  (step [_ gamestate time]
    (.step world time)
    (.render world)
    (doseq [e (:entities gamestate)])
    gamestate
    ))

(defn create-physics-engine []
  (let [world (js/Physics.)
        engine
        (PhysicsJSEngine. world (atom #{}))
        renderer (.renderer js/Physics "canvas"
                            (clj->js
                             {:el "canvas"
                              :width 1000
                              :height 500
                              :meta true
                              :sytles {:circle
                                       {:strokeStyle
                                        "hsla(60, 37%, 17%, 1)"
                                        :lineWidth 1
                                        :fillStyle
                                        "hsla(60, 37%, 57%, 0.8)"
                                        :angleIndicator
                                        "hsla(60, 37%, 17%, 0.4)"
                                        }}}))
        ;; Just get this ball to bounce or somfin
        ball (.body js/Physics "circle" (clj->js
                                         {:x 0
                                          :y 300
                                          :vx 0.2
                                          :vy 0.01
                                          :radius 20}))]
    ;; world.add( Physics.behavior('body-impulse-response') );
    (.add world (.behavior js/Physics "body-impulse-response"))
    (.add world renderer)
    (.add world ball)
    engine
    ))

;; (defn step [gamestate engine t]
;;   ()
;;   (.step engine t)
;;   gamestate)

