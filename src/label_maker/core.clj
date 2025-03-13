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

(defn sizes-to-resize [n intial step]
  (take n (iterate (partial * step) intial)))
(comment (sizes-to-resize 4 60 1.25))

(defn fit-amount
  "How many pics fits in the whole page"
  [pic-w pic-h]
  (let [w-count    (/ W pic-w)
        h-count    (/ H pic-h)
        fit-amount (apply min [w-count h-count])]
    (int fit-amount)))

(defn calculate-positions* [pic-w pic-h amount]
  (reduce (fn [acc n]
            (conj acc
                  [(* n pic-w) (* n pic-h)]))
          []
          (range amount)))

(defn calculate-positions
  "Returns xs and ys for the picture"
  [{:keys [w h] :as _image}]
  (let [amount (fit-amount w h)]
    (calculate-positions* w h amount)))
(comment (calculate-positions {:w 50 :h 10}))

(defn calculate-ys
  "Returns xs and ys for the picture"
  [images]
  (reduce
   (fn [acc item]
     (conj acc
           (assoc item :y
                  (reduce + (map :h acc)))))
   []
   images))

(defn update-state-ys [state]
  (update  state :images calculate-ys))

(defn setup-fn [picture]
  (q/frame-rate 10)
  (q/color-mode :hsb)
  (let [sizes  (sizes-to-resize 10 40 1.5) ;; FIXME configure
        images (for [size sizes]
                 {:image (q/load-image (or picture "resources/test.png"))
                  :size  size})]
    {:images images}))

(defn the-key-handler [state k]
  (assoc state :done (= ENTER (:key-code k))))

(defn- resize-images! [state]
  (run! (fn [{:keys [image size]}]
          (q/resize image size 0))
        (:images state))
  state)

(defn- assoc-position [image-map]
  (assoc image-map :positions (calculate-positions image-map)))

(defn- assoc-positions [images]
  (mapv assoc-position images))

(defn- update-images-positions [state]
  (update state :images assoc-positions))

(defn- assoc-w-h [{:keys [image], :as image-map}]
  (assoc image-map
         :w (.width image)
         :h (.height image)))

(defn- assoc-w-hs [images]
  (mapv assoc-w-h images))

(defn- update-images-w-hs [state]
  (update state :images assoc-w-hs))

(defn mark-ready [state]
  (assoc state :ready true))

(defn update-fn [{:keys [images ready] :as state}]
  (let [loaded? (every? q/loaded? (map :image images))]
    (cond
      ready   state
      loaded? (-> state
                  resize-images!
                  update-images-w-hs
                  update-images-positions
                  update-state-ys
                  mark-ready)
      :else   state)))

(defn draw-labels [state]
  (q/background 255)
  (mapv (fn draw [{:keys [image y w]}]
          (run!
           #(q/image image % y)
           (range 0 W w)))
        (:images state)))

(defn draw-fn [state]
  (when (:ready state)
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
    :setup (partial setup-fn picture-path)
                                        ; update is called on each iteration before draw.
    :update update-fn
    :draw draw-fn
    :features [:keep-on-top]
    :key-pressed the-key-handler
                                        ; This sketch uses functional-mode middleware.
                                        ; Check quil wiki for more info about middlewares and particularly
                                        ; fun-mode.
    :middleware [m/fun-mode m/pause-on-error]))

(comment
  (-main "/home/danielhabib/Downloads/pensador.jpg")
  (-main))
