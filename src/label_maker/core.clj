(ns label-maker.core
  (:require [mikera.image.core :refer [resize load-image-resource show new-image]])
  (:gen-class))

(defn load [nome-arquivo]
  (load-image-resource nome-arquivo))

(defn make-labels [filename w h]
  (let [image (load filename)]
    (resize image w h)))

(defn -main
  [& args]
  (show (make-labels "test.png" 300 100)))
