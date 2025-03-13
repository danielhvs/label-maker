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
  [{:keys [w h] :as img}]
  (let [amount (fit-amount w h)]
    (calculate-positions* w h amount)))
(comment (calculate-positions {:w 50 :h 10}))

(defn position [size]
  (let [w (:w size)
        h (:h size)]
    (merge size
           {:x (- W w)
            :y (- H h)})))

(defn positions [sizes]
  (map position sizes))

(def widths
  (for [n (range 1 7)]
    (let [w (* n (/ W 8.0))]
      {:w w})))

(defn prev-key [kw]
  (keyword
   (str "w"
        (dec (Integer/valueOf (apply str (rest (name kw))))))))

(def arbitrary-positions
  "x y w h"
  (let [first-w 90
        f1      2.25
        f2      1.5
        f3      1.25]
    [[0 0 first-w 0]
     [105 0 (* first-w f1) 0]
     [285 0 (* first-w f1 f2) 0 0]
     [0 385 (* first-w f1 f2 f3) 0 0]]))

(def arbitrary-positions
  "x y w h"
  (let [w-and-ys
        (mapv #(select-keys % [:w :y])
              (let [the-map
                    {:w1 74.375, :w2 148.75, :w3 223.125, :w4 297.5, :w5 371.875, :w6 446.25}]
                (reduce-kv (fn [acc k v]
                             (conj acc
                                   (merge
                                    {k v}
                                    {:w v}
                                    {:y
                                     (or ((prev-key k) (first (filter (prev-key k) acc))) ;; FIXME sum up all the prev-keys
                                         0)})))
                           []
                           the-map)))]
    (mapv
     (fn [{:keys [y w]}]
       [0 y w 0])
     w-and-ys)))

(defn setup-fn [picture]
  (q/frame-rate 10)
  (q/color-mode :hsb)
  (let [sizes  (sizes-to-resize 4 60 1.25)
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

(defn update-fn [{:keys [images ready], :as state}]
  (let [loaded? (every? q/loaded? (map :image images))]
    (println "loaded?:" loaded?)
    (println "ready:" ready)
    (cond
      ready   state
      loaded? (-> state
                  resize-images!
                  update-images-w-hs
                  update-images-positions
                  mark-ready)
      :else   state)))

(defn draw-labels [state]
  (q/background 255)
  (when (:ready state)
    (mapv (fn draw [{:keys [image positions]}]
            (run!
             (fn [position]
               (let [[x y] position]
                 (q/image image x y)))
             positions))
          (:images state))))

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
