(ns luminosity-analyser.utils
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.core.matrix :as matrix]))

(defn take-from [data a b]
  (drop a (take b data)))

(defn read-data [filename]
  (with-open [reader (io/reader filename)]
    (doall
     (csv/read-csv reader))))

(defn clip-data [data config]
  (let [fmt-config (config 0)]
    (map #(take-from % (:data/start-col fmt-config) (:data/end-col fmt-config)) (rest data))))

(defn label-n [n]
  (str "Cell_" n))

(defn label-resp [name]
  (str name "_resp?"))

(defn label-peak [name]
  (str name "_peak"))

(defn prettify-data [data config]
  (let [out-config (config 0)
        no-cols (- (:data/end-col out-config) (:data/start-col out-config))
        first-col (range 1 (inc (matrix/column-count (:normalised-data data))))
        label-row (conj (map label-n (range 1 (inc no-cols))) (:data/x-label out-config))
        fmt-norm (conj (:normalised-data data) first-col)
        fmt-norm-labels (concat [label-row] (matrix/columns fmt-norm))
        fmt-response-labels (map #(conj %1 (label-resp (:chemical/name %2))) (:responses data) (rest config))
        fmt-peak-labels (map #(conj %1 (label-peak (:chemical/name %2))) (:peak-maxes data) (rest (rest config)))]
    (concat fmt-norm-labels fmt-response-labels fmt-peak-labels)))

(defn write-data [filename data]
  (with-open [writer (io/writer filename)]
    (csv/write-csv writer data)))


