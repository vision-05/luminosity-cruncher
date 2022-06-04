(ns luminosity-analyser.processing
  (:gen-class)
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.matrix.stats :as ms]
            [luminosity-analyser.utils :as utils]))

(defn subset [data config kw-start kw-end]
  (map #(utils/take-from data (kw-start %) (kw-end %)) config))

(defn peak-subsets [data config kw-start kw-end]
  (map #(utils/take-from %2 (kw-start %1) (kw-end %1)) config data))

(defn col-means
  "matrix column means, data already formatted"
  [data]
  (map ms/mean (matrix/columns data)))

(defn col-means-n-rows [data n]
  (col-means (take n data)))

(defn col-sd [data]
  (matrix/sqrt (ms/variance data)))

(defn remove-background [data background]
  (map matrix/sub data (map #(Double/parseDouble %) background)))

(defn remove-baseline [data n-rows]
  (map matrix/sub (matrix/columns data) (col-means-n-rows data n-rows)))

(defn threshold-n [data n multiplier]
  (let [sample (take n data)]
    (matrix/add (col-means sample) (matrix/mul (col-sd sample) multiplier))))

(defn get-response [peak-data data thresholds]
  (map #(if (> %1 %2) %3 nil) (ms/mean peak-data) thresholds data))

(defn binary-response [responses]
  (map #(if (= nil %) 0 1) responses))

(defn response-percent [responses]
  (* 100 (/ (reduce + (binary-response responses)) (count responses))))

(defn col-max [data]
  (map #(matrix/emax %) (matrix/columns data)))

(defn normalise-fmax [norm-data col-data]
  (map matrix/div col-data (col-max norm-data)))

(defn process [data config]
  (let [corrected (remove-baseline data 10) ;;column format
        thresholds (threshold-n (matrix/columns corrected) 10 5) ;;column format
        subsets (subset (matrix/columns corrected) config :chemical/time-start :chemical/time-end) ;;row format
        peaks (peak-subsets subsets config :chemical/peak-start :chemical/peak-end) ;;row format
        responses (map #(get-response %1 (matrix/columns %2) thresholds) peaks subsets) ;;column format
        percentage-responses (map response-percent responses) ;;column format
        normalised (normalise-fmax (first subsets) corrected) ;;drug to normalise against MUST be first, column format
        normalised-subsets (subset (matrix/columns normalised) (rest config) :chemical/time-start :chemical/time-end) ;;drop the normalising drug, only need other drug peaks, row format
        normalised-peaks (peak-subsets normalised-subsets (rest config) :chemical/peak-start :chemical/peak-end) ;;row format
        peak-max (col-max normalised-peaks)] ;;column format
    (prn percentage-responses)
    {:responses responses
     :percent-response percentage-responses
     :normalised-data (matrix/columns normalised)
     :peak-maxes peak-max}))
