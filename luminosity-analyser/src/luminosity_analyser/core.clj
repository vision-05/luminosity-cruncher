(ns luminosity-analyser.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.core.matrix :as matrix]
            [luminosity-analyser.utils :as utils]
            [luminosity-analyser.processing :as processing]))

(defn -main
  "I don't do a whole lot ... yet."
  [infile outfile f-config]
  (let [config (edn/read-string (slurp f-config))
        p (println config)
        clip-config (rest config)
        data (utils/read-data infile)
        clip-data (map #(map (fn [x] (Double/parseDouble x)) %) (utils/clip-data data config))
        sub-background (processing/remove-background clip-data (last (matrix/columns (rest data))))]
    (utils/write-data outfile (:normalised-data (processing/process sub-background clip-config)))))
