(ns label-maker.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn load-pos []
  [
   [0 0] [0 50] [0 100]
   [50 0] [50 50] [50 100]
   [100 0] [100 50] [100 100]
   [150 0] [150 50] [150 100]
   ])

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:image (q/load-image "resources/test.png")
   :all-pos (load-pos)
   })

(defn update-state [state]
  {:image (:image state)
   :all-pos (pop (:all-pos state))
   :pos (peek (:all-pos state))
   })

(defn draw-state [state]
  (let [img (:image state)]
    (when (q/loaded? img)
      (when-let [p (:pos state)]
        (q/image img (first p) (second p)))
)))

(q/defsketch label-maker
  :title "You spin my circle right round"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
