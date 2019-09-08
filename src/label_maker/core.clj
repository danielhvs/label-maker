(ns label-maker.core
  (:require [mikera.image.core :refer [load-image-resource show new-image]])
  (:gen-class))

(defn carrega [nome-arquivo]
  (load-image-resource nome-arquivo))

(defn -main
  [& args]
  (show (carrega "test.png") :title "oi"))
