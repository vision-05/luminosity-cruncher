(ns luminosity-analyser.specs
  (:require [spec.alpha :as spec]))

(spec/def :chemical/name string?)
(spec/def :chemical/time-start pos-int?)
(spec/def :chemical/time-end pos-int?)
(spec/def :chemical/peak-start pos-int?)
(spec/def :chemical/peak-end pos-int?)
(spec/def :chemical/chemical (spec/keys :req
                                     [:chemical/name
                                      :chemical/time-start
                                      :chemical/time-end
                                      :chemical/peak-start
                                      :chemical/peak-end]))

