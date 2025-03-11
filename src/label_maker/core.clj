(ns label-maker.core
  (:require
   [quil.core       :as q]
   [quil.middleware :as m])
  (:gen-class))

;; A4 paper size
(defn mm-to-px
  [mm]
  (/ (* mm 72) 25.4))

(def W (int (mm-to-px 210)))
(def H (int (mm-to-px 297)))
(def ENTER 10)

;; W 595
;; H 841

(defn sizes []
  (let [w-count 4
        w-size  (/ W w-count)
        w-mults [1.0 2.0]
        h-count 8
        h-size  (/ H h-count)
        h-mults [1.0 2.0 4.0]
        the-map {:w-size (mapv #(* % h-size) h-mults)
                 :h-size (mapv #(* % w-size) w-mults)}]
    (let [[ws hs] (vals the-map)]
      (for [w ws
            h hs]
        {:w w
         :h h}))))

(defn position [size]
  (let [w   (:w size)
        h   (:h size)
        res (merge size
                   {:x (- W w)
                    :y (- H h)})]
    (println "res:" res)
    res))

(defn positions [sizes]
  (println "sizes:" sizes)
  (map position sizes))

(comment
  (positions (sizes)))

(def posss
  "x y w h"
  [[0 0 10 10]])

(defn setup [picture]
  (q/frame-rate 10)
  (q/color-mode :hsb)
  (let [the-imgs (repeatedly (count posss) #(q/load-image (or picture "resources/test.png")))]
    {:images (mapv (fn [img] {:img img}) the-imgs)}))

(defn the-key-handler [state k]
  (assoc state :done (= ENTER (:key-code k))))

(defn update-images  [imgs]
  (mapv (fn [img [x y w h]]
          (merge img {:w w :x x :y y :h h}))
        imgs
        posss))

#_(:ready-to-draw state)

(defn check-loaded  [img]
  (if (q/loaded? (:img img))
    (assoc img :ready? true)
    img))

;; (q/resize img w h)

(defn update-fn [state]
  (let [imgs    (:images state)
        loaded? (count (map :ready? (map check-loaded imgs)))]
    (if (= loaded? (count posss))
      (let [new-state (-> state
                          (update :images update-images)
                          (assoc :ready-to-draw true))]
        (when-not (:resized state)
          (run! (fn [{:keys [img w h]}] (q/resize img w h))
                (:images new-state)))
        (assoc new-state :resized true))
      {:images  (:images state)
       :all-pos (positions (sizes))
       :done    (:done state)})))

(defn draw-labels [state]
  (let [imgs (:images state)]
    (do
      (q/background 255)
      (mapv (fn draw [{:keys [img x y]}]
              (println "draw:" draw)
              (println "y:" y)
              (println "x:" x)
              (q/image img x y))
            imgs))))

(defn draw [state]
  (when (:ready-to-draw state)
    (when (:done state)
      (q/do-record (q/create-graphics W H :pdf "out.pdf")
                   (draw-labels state))
      (q/exit))
    (draw-labels state)))

(defn -main
  "Args: file-path (optional)"
  [& [picture-path]]
  (q/defsketch label-maker
    :title "Label Maker"
    :size [W H]
                                        ; setup function called only once, during sketch initialization.
    :setup (partial setup picture-path)
                                        ; update is called on each iteration before draw.
    :update update-fn
    :draw draw
    :features [:keep-on-top]
    :key-pressed the-key-handler
                                        ; This sketch uses functional-mode middleware.
                                        ; Check quil wiki for more info about middlewares and particularly
                                        ; fun-mode.
    :middleware [m/fun-mode m/pause-on-error]))

(comment
  (-main "/home/danielhabib/Downloads/visualization.png")
  (-main))
