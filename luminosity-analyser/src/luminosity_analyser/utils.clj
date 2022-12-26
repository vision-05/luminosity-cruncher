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

(defn int->names [n names]
  (let [na (vec names)]
    (apply str (map #(na %) (map (fn [x] (Character/getNumericValue x)) (str n))))))

(defn prettify-data [data config]
  (let [out-config (config 0)
        no-cols (- (:data/end-col out-config) (:data/start-col out-config))
        first-col (range 1 (inc (matrix/column-count (:normalised-data data))))
        label-row (conj (map label-n (range 1 (inc no-cols))) (:data/x-label out-config))
        names (map #(:chemical/name %) (rest config))
        fmt-norm (conj (:normalised-data data) first-col)
        fmt-norm-labels (concat [label-row] (matrix/columns fmt-norm))
        fmt-response-labels (map #(vector (float %2) (label-resp (:chemical/name %1))) (rest config) (:percent-response data))
        fmt-n-comb (map #(vector (int->names (first %) names) (float (last %))) (:combination-response-n data))
        fmt-perc-comb (map #(vector (int->names (first %) names) (float (last %))) (:combination-response-perc data))]
    (prn fmt-perc-comb)
    (prn fmt-response-labels)
       (concat fmt-response-labels fmt-norm-labels fmt-n-comb fmt-perc-comb [[(:count data)]])))

(defn write-data [filename data]
  (with-open [writer (io/writer filename)]
    (csv/write-csv writer data)))


