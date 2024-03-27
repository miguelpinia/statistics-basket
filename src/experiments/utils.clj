(ns experiments.utils
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [incanter.stats :as stats]))

(defn json->map [file-name]
 (->> file-name
      io/reader
      json/parse-stream
      walk/keywordize-keys))

(defn simple-stats [data]
  {:ci       (stats/simple-ci data)
   :mean     (stats/mean data)
   :sd       (stats/sd data)
   :variance (stats/variance data)
   :median   (stats/median data)})

(defn map->json
  ([file-output data] (map->json file-output data true))
  ([file-output data pretty]
   (with-open [writer (io/writer file-output)]
     (json/generate-stream data writer {:pretty pretty}))))

#_(to-pretty-json "data/queue/inner/64/analysis.json" (analyze-entries "extract_insert" 64 "data/queue/inner/64/"))
