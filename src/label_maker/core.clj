(ns label-maker.core
  (:require [mikera.image.core :refer [show new-image]])
  (:gen-class))

(defn -main
  [& args]
  (show (new-image 1 1)))
