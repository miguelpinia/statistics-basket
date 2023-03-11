(ns stats.queue-analysis
  (:require [clojure.data.json :as json]
            [incanter.core :as i]
            [incanter.stats :as s]
            [incanter.charts :as c]
            [clojure.walk :refer [keywordize-keys]]
            [clojure.pprint :refer [pprint]]
            [stats.utils :as u]
            [clojure.java.io :as io]
            [clojure.walk :as walk]))


#_(map #(keyword (str %)) (range 1 65))

(defn get-stats [proc data]
  {:core     proc
   :mean     (s/mean data)
   :sd       (s/sd data)
   :variance (s/variance data)
   :median   (s/median data)
   :ci       (s/simple-ci data)})

(defn get-stats-algo-core [core algo-filtered]
  (get-stats (Integer/parseInt (name core)) (get algo-filtered core)))

(defn get-stats-algo [cores algo-filtered]
  (map #(get-stats-algo-core % algo-filtered) cores))

(defn analyze-entries [type cores path]
  (let [
        files-enq-deq    (->> path
                              io/file
                              file-seq
                              (filter #(not (.isDirectory %)))
                              (filter #(not= (.getName %) "analysis.json")))
        data-transformed (->> files-enq-deq (map #(-> %
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
     :results (map #(identity {:algorithm %
                             :stats     (get-stats-algo entries (get pre-processed %))}) algorithms)}))

(defn to-json [output-file result]
  (spit (java.io.File. output-file)
        (json/write-str result)))

(defn to-pretty-json [output-file result]
  (spit (java.io.File. output-file)
        (with-out-str (json/pprint-json result))))
;; (to-pretty-json "data/queue/enq_deq/64/analysis.json" (analyze-entries "enq_deq" 64 "data/queue/enq_deq/64"))
