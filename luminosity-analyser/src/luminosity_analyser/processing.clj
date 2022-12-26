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
    (matrix/add (col-means sample) (matrix/mul (col-sd sample) multiplier)))) ;mean + sd*5

(defn get-response [peak-data data thresholds]
  (map #(if (> %1 %2) %3 nil) (ms/mean peak-data) thresholds data))

(defn binary-response [responses]
  (map #(if (= nil %) 0 1) responses))

(defn response-percent [responses]
  (* 100 (/ (reduce + responses) (count responses))))

(defn col-max [data]
  (map #(matrix/emax %) (matrix/columns data)))

(defn normalise-fmax [norm-data col-data]
  (map matrix/div col-data (col-max norm-data)))

(defn resp-n-percent [col]
  (apply str (remove zero? (map #(if (= %1 1) %2 0) col (range (count col))))))

(defn process [data config]
  (let [corrected (remove-baseline data 10) ;;column format
        thresholds (threshold-n (matrix/columns corrected) 10 5) ;;column format
        subsets (subset (matrix/columns corrected) config :chemical/time-start :chemical/time-end) ;;row format
        peaks (peak-subsets subsets config :chemical/peak-start :chemical/peak-end) ;;row format
        responses (map #(get-response %1 (matrix/columns %2) thresholds) peaks subsets) ;;column format
        resp-column (remove #(= 0 (% 0)) (matrix/columns (map binary-response responses)))
        num-resp-column (map resp-n-percent resp-column)
        fmt-n-r-column (into (sorted-map) (map #(vector (first %) (count %)) (partition-by identity (sort (map (fn [x] (if (= "" x) 0 (Integer/parseInt x))) num-resp-column)))))
        perc-n-r-column (into (sorted-map) (map #(vector (first %) (* 100 (/ (last %) (count resp-column)))) fmt-n-r-column))
        resp-rows (matrix/columns resp-column)
        percentage-responses (map response-percent resp-rows) ;;column format
        normalised (normalise-fmax (first subsets) corrected) ;;drug to normalise against MUST be first, column format
        normalised-subsets (subset (matrix/columns normalised) (rest config) :chemical/time-start :chemical/time-end) ;;drop the normalising drug, only need other drug peaks, row format
        normalised-peaks (peak-subsets normalised-subsets (rest config) :chemical/peak-start :chemical/peak-end) ;;row format
        peak-max (map col-max normalised-peaks)] ;;column format
    (prn percentage-responses)
    (prn (count resp-column))
    (prn fmt-n-r-column)
    (prn perc-n-r-column)
    {:responses resp-rows
     :resp-n resp-column
     :combination-response-n fmt-n-r-column
     :combination-response-perc perc-n-r-column
     :percent-response percentage-responses
     :normalised-data normalised
     :peak-maxes peak-max
     :count (count resp-column)}))
