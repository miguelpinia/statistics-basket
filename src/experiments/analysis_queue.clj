(ns experiments.analysis-queue
  (:require [experiments.utils :as u]
            [clojure.java.io :as io]))

(defn get-stats [cores map-algo-filtered]
  (map (fn [core]
       (let [core-int (-> core name Integer/parseInt)]
         (-> map-algo-filtered
             (get core)
             u/simple-stats
             (assoc :core core-int))))
     cores))

(defn analyze-entries [type cores path]
  (let [proc-nums  (map #(keyword (str %)) (range 1 (inc cores)))
        data       (->> path
                        io/file
                        .listFiles
                        (filter #(.isFile %))
                        (filter #(not= (.getName %) "analysis.json"))
                        (map u/json->map))
        algorithms (map #(-> % :algorithm keyword) data)
        pre-proc   (reduce (fn [acc entry]
                             (assoc acc
                                    (keyword (:algorithm entry))
                                    (:results entry)))
                           {}
                           data)]
    {:type    type
     :results (map #(identity
                   {:algorithm %
                    :stats     (get-stats proc-nums (get pre-proc %))})
                 algorithms)}))

(defn analysis [type cores file-output dir-input]
  (u/map->json file-output
               (analyze-entries type cores dir-input)
               true))
