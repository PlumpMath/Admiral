(ns admiral.render.core)

;; Util
(defn get-canvas-element
  [id]
  (.getElementById js/document id))

(defn reset-window-size!
  [canvas]
  (let [width (.-innerWidth js/window)
        height (.-innerHeight js/window)]
    (aset canvas "width" width)
    (aset canvas "height" height)))

(defn to-color [& rgbas]
  (let [csv (apply str (interpose ", " rgbas))]
    (str "rgb(" csv ")")))

;; Drawing Primitives
(def twopi (* 2 (.-PI js/Math)))

(defn draw-rectangle
  [ctx color pos w h]
  (let [[x y] pos]
    (aset ctx "fillStyle" (apply to-color color))
    (.fillRect ctx x y w h)))

(defn draw-circle
  [ctx color pos r]
  (let [[x y] pos]
  	(aset ctx "fillStyle" (apply to-color color))
  	(.beginPath ctx)
  	(.arc ctx x y r 0 twopi)
  	(.closePath ctx)
  	(.fill ctx)))

(defn draw-line [ctx color width points]
  (let [[startx starty] (first points)]
    (.beginPath ctx)
    (.moveTo ctx startx starty)
    (doseq [[x y] (rest points)]
      (.lineTo ctx x y))
    (.closePath ctx)
    (aset ctx "lineWidth" width)
    (.stroke ctx)))

;; Drawing Entities
(defmulti draw-entity :model)

(defmethod draw-entity :ship [{:keys [color pos facing]} ctx]
  
  )

;; Renderer
(defprotocol Renderer
  "A context used to render admiral to the screen."
  (render [this world] "Renders the world to the screen."))

(deftype Canvas [context]
  Renderer
  (render [_ gamestate]
    (draw-circle context [200 0 0]
                 (get-in gamestate [:entities :only-ship :pos])
                 10
                 )
    ))

(defn create-canvas-renderer
  "Creates a renderer from a canvas id."
  [canvas-id]
  (let [canvas (get-canvas-element canvas-id)
        context (.getContext canvas "2d")]

    ;; Resize canvas when screen resizes
    ;; Not sure if this is what I want when I have UI.
    (aset js/window "onresize"
          (fn [e] (reset-window-size! canvas)))
    (reset-window-size! canvas)
    
    (Canvas. context)))
