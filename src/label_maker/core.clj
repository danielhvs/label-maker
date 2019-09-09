(ns label-maker.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; A4 paper size
(defn mm-to-px
  [mm]
  (/ (* mm 72) 25.4))

(def W (int (mm-to-px 210)))
(def H (int (mm-to-px 297)))
(def QTD-W 9)
(def QTD-H 12)

(defn calculate-pos [w h qtd-w qtd-h size-w size-h]
  (let [
        h-size (max size-h (quot h qtd-h))
        w-size (max size-w (quot w qtd-w))
        ]
    (let [ys (filter #(= 0 (rem % h-size)) (range h))
          xs (filter #(= 0 (rem % w-size)) (range w))]
      (for [x xs y ys]
        [x y])))) 

(defn setup []
  ; Set frame rate frames per second.
  (q/frame-rate 10)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:image (q/load-image "resources/test.png")})

(defn update [state]
  (let [img (:image state)
        next {:image img}]
    (if (q/loaded? img)
      {:image img :all-pos (calculate-pos W H QTD-W QTD-H (.width img) (.height img))}
      {:image img}))
  )

(defn draw-labels [state]
  (let [img (:image state)]
    (when-let [all-pos (:all-pos state)]
      (doall
       (map #(q/image img (first %) (second %)) all-pos)))))

(defn draw [state]
  (when (:all-pos state)
    (q/do-record (q/create-graphics W H :pdf "out.pdf")
                 (draw-labels state))
    (q/exit)))

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
