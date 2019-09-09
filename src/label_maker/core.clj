(ns label-maker.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; A4 paper size
(defn mm-to-px
  [mm]
  (/ (* mm 72) 25.4))
(def W (int (mm-to-px 210)))
(def H (int (mm-to-px 297)))

(defn calculate-pos [w h qtd-w qtd-h]
  (let [
        h-size (quot h qtd-h)
        w-size (quot w qtd-w)
        ]
    (let [ys (filter #(= 0 (rem % h-size)) (range h))
          xs (filter #(= 0 (rem % w-size)) (range w))]
      (for [x xs y ys]
        [x y])))) 

(defn setup []
  ; Set frame rate frames per second.
  (q/frame-rate 60)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:image (q/load-image "resources/test.png")
   :all-pos (into [] (calculate-pos W H 3 3))
   })

(defn update [state]
  {:image (:image state)
   :all-pos (pop (:all-pos state))
   :pos (peek (:all-pos state))
   })

(defn draw [state]
  (let [img (:image state)]
    (when (q/loaded? img)
      (when-let [p (:pos state)]
        (q/image img (first p) (second p)))
)))

(q/defsketch label-maker
  :title "Label Maker"
  :size [W H]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update is called on each iteration before draw.
  :update update
  :draw draw
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
