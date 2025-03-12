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
(comment (calculate-positions {:w 5000 :h 1000}))

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

(do
  (defn the-widths []
    (reduce (fn [acc n]
              (println "n:" n)
              (let [w (* n (/ W 8.0))]
                (assoc acc (keyword (str "w" n))  w)))
            {}
            (range 1 7)))
  (the-widths))

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
  (let [sizes (sizes-to-resize 4 60 1.25)]
    {:image           (q/load-image (or picture "resources/test.png"))
     :sizes-to-resize sizes
     :images          (repeatedly (count sizes) (q/load-image (or picture "resources/test.png")))}))

(defn the-key-handler [state k]
  (assoc state :done (= ENTER (:key-code k))))

(defn update-images [imgs]
  (mapv (fn [img [x y w h]]
          (merge img
                 {:img-w (.width (:img img))
                  :img-h (.height (:img img))}
                 {:w w :x x :y y :h h}))
        imgs
        arbitrary-positions))

(defn- maybe-resize-images-old [state]
  (let [next-state (let [new-state (-> state
                                       (update :images update-images) ;; assoc x and y
                                       (assoc :ready-to-draw true))]
                     (when-not (:resized state)
                       (run! (fn [{:keys [img w h]}]
                               (q/resize img w h))
                             (:images new-state)))
                     (assoc new-state :resized true))]
    (println "next-state:" next-state)
    next-state))

(defn- calculate-state-positions [{:keys [image resized] :as state}]
  (let [next-state
        (let [resize-to 60
              bla       {:w resize-to
                         :h 60}
              positions (calculate-positions bla)]
          (when-not resized
            (q/resize image resize-to 0))
          (assoc state :positions positions :resized true))]
    (println "next-state:" next-state)
    next-state))

(defn update-fn [state]
  (let [images  (:images state)
        loaded? (every? q/loaded? images)]
    (cond
      loaded? (maybe-resize-images-old state)
      :else   state)))

#_(defn update-fn [state]
    (let [image   (:image state)
          loaded? (q/loaded? image)]
      (cond
        (:resized state) state
        loaded?          (calculate-state-positions state)
        :else            state)))

(defn draw-labels [state]
  (q/background 255)
  (when-let [positions (:positions state)]
    (println "draw-labels state:" positions)
    (mapv (fn draw [[x y]]
            (q/image (:image state) x y))
          positions)))

(defn draw-fn [state]
  (when (:resized state)
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
