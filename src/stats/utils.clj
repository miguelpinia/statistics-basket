(ns stats.utils
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]
            [incanter.stats :as s]))

(defn json-to-map [file-name]
  (->> file-name
       slurp
       json/read-str
       walk/keywordize-keys))

(defn simple-stats [data]
  {:ci       (s/simple-ci data)
   :mean     (s/mean data)
   :sd       (s/sd data)
   :variance (s/variance data)
   :median   (s/median data)})
