(ns luminosity-analyser.utils
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn take-from [data a b]
  (drop a (take b data)))

(defn read-data [filename]
  (with-open [reader (io/reader filename)]
    (doall
     (csv/read-csv reader))))

(defn clip-data [data config]
  (let [fmt-config (config 0)]
    (map #(take-from % (:data/start-col fmt-config) (:data/end-col fmt-config)) (rest data))))

(defn write-data [filename data]
  (with-open [writer (io/writer filename)]
    (csv/write-csv writer data)))


