(ns stats.analysis.llic
  (:require [clojure.data.json :as json]
            [incanter.core :as i]
            [incanter.stats :as s]
            [incanter.charts :as c]
            [clojure.walk :refer [keywordize-keys] :as walk]
            [clojure.pprint :refer [pprint]]
            [stats.utils :as u]
            [clojure.java.io :as io]))

(defn get-stats [proc data]
  {:core     proc
   :mean     (s/mean data)
   :sd       (s/sd data)
   :variance (s/median data)
   :ci       (s/simple-ci data)})

(defn get-stats-algo-core [core algo-filtered]
  (get-stats (Integer/parseInt (name core)) (get algo-filtered core)))

(defn get-stats-algo [cores algo-filtered]
  (map #(get-stats-algo-core % algo-filtered) cores))

(defn analyze-entries [type cores path]
  (let [files-llic       (->> path
                              io/file
                              .listFiles
                              (filter #(.isFile %))
                              (filter #(not= (.getName %) "analysis.json")))
        _ (println files-llic)
        data-transformed (->> files-llic
                              (map #(-> %
                                      slurp
                                      json/read-str
                                      walk/keywordize-keys)))
        entries          (map #(keyword (str %)) (range 1 (inc cores)))
        algorithms       (map #(-> % :algorithm keyword) data-transformed)
        pre-processed    (reduce (fn [acc entry]
                                   (assoc acc
                                          (keyword (:algorithm entry))
                                          (:results entry)))
                                 {}
                                 data-transformed)
        algo             (first algorithms)
        algo-filtered    (algo pre-processed)]
    {:type    type
     :results (map #(identity
                   {:algorithm %
                    :stats     (get-stats-algo entries (get pre-processed %))})
                 algorithms)}))

(defn to-json [output-file result]
  (spit (java.io.File. output-file)
        (json/write-str result)))

(defn to-pretty-json [output-file result]
  (spit (java.io.File. output-file)
        (with-out-str (json/pprint-json result))))

#_(to-pretty-json "data/LLIC/insert_extract/64/analysis.json" (analyze-entries "extract_insert" 64 "data/LLIC/insert_extract/64"))

#_(to-pretty-json "data/queue/inner/32/analysis.json" (analyze-entries "extract_insert" 32 "data/queue/inner/32/"))

#_(to-pretty-json "data/queue/inner/64/analysis.json" (analyze-entries "extract_insert" 64 "data/queue/inner/64/"))
