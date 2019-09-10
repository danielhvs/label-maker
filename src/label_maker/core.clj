(ns label-maker.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; A4 paper size
(defn mm-to-px
  [mm]
  (/ (* mm 72) 25.4))

(def W (int (mm-to-px 210)))
(def H (int (mm-to-px 297)))
(def ENTER 10)

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
  {:image (q/load-image "resources/test.png")
   :qtd-w 1
   :qtd-h 1})

(defn key-to-offset-w [key]
  (case (:key key)
    :left dec
    :right inc
    identity))

(defn key-to-offset-h [key]
  (case (:key key)
    :up dec
    :down inc
    identity))

(defn the-key-handler [state k]
  (assoc state 
    :qtd-w (max 1 ((key-to-offset-w k) (:qtd-w state)))
    :qtd-h (max 1 ((key-to-offset-h k) (:qtd-h state)))
    :done (= ENTER (:key-code k))))

(defn update [state]
  (let [img (:image state)
        next {:image img}
        qtd-w (:qtd-w state)
        qtd-h (:qtd-h state)]
    (if (q/loaded? img)
      {:image img 
       :all-pos (calculate-pos W H qtd-w qtd-h (.width img) (.height img))
       :qtd-w qtd-w
       :qtd-h qtd-h
       :done (:done state)}
      {:image img
       :qtd-w qtd-w
       :qtd-h qtd-h
       :done (:done state)})))

(defn draw-labels [state]
  (let [img (:image state)]
    (when-let [all-pos (:all-pos state)]
      (do
        (q/background 255)
        (doall
         (map #(q/image img (first %) (second %)) all-pos))))))

(defn draw [state]
  (when (:all-pos state)
    (if (:done state)
      (do
        (q/do-record (q/create-graphics W H :pdf "out.pdf")
                     (draw-labels state))
        (q/exit)) 
      (draw-labels state))))

(q/defsketch label-maker
  :title "Label Maker"
  :size [W H]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update is called on each iteration before draw.
  :update update
  :draw draw
  :features [:keep-on-top]
  :key-pressed the-key-handler
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode m/pause-on-error])
