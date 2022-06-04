(ns luminosity-analyser.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [luminosity-analyser.core :refer :all]))

(defn gen-array [n m]
  (gen/vector (gen/vector gen/double m) n))

(defn  )
