(ns stats.sat
  (:require [stats.utils :as utils]
            [incanter.charts :as c]
            [incanter.core :as i]
            [incanter.stats :as s]
            [clojure.pprint :as pp]
            [clojure.data.json :as j]
            [clojure.data.json :as json]
            [clojure.string :as str]))


(def file-01 "data/ex1periment-sat-01.json")
(def file-02 "data/experiment-sat-02.json")
(def last-file "data/experiment-brute-sat-12_06_2023-04:00:16-stats.json")

(def count-50 "data/50.json")
(def count-100 "data/100.json")
(def count-1000 "data/1000.json")
(def count-5000 "data/5000.json")
(def count-10000 "data/10000.json")
(def count-2-50 "data/experiment-brute-50.json")
(def count-2-100 "data/experiment-brute-100.json")
(def count-2-250 "data/experiment-brute-250.json")
(def count-2-500 "data/experiment-brute-500.json")
(def count-2-1000 "data/experiment-brute-1000.json")
(def count-2-2500 "data/experiment-brute-2500.json")
(def count-2-5000 "data/experiment-brute-5000.json")
(def stats-sat "data/statistical.json")
(def stats-sat-10 "data/stats-sat-10.json")
(def stats-sat-50 "data/stats-sat-50.json")
(def stats-sat-100 "data/stats-sat-100.json")
(def stats-sat-250 "data/stats-sat-250.json")
(def multi-sat-50 "data/sat-multiplicity-50.json")
(def multi-sat-100 "data/sat-multiplicity-100.json")
(def multi-sat-250 "data/sat-multiplicity-250.json")
(def multi-sat-500 "data/sat-multiplicity-500.json")
(def multi-sat-1000 "data/sat-multiplicity-1000.json")
(def multi-sat-2500 "data/sat-multiplicity-2500.json")
(def multi-sat-5000 "data/sat-multiplicity-5000.json")


#_(def count-2-10000 "data/experiment-brute-10000.json")

(defn percentage [alg-vals]
  (let [space (:space alg-vals)
        rs (map #(keyword (str %)) (range 1 65))]
    (map (fn [idx] (let [[v] (idx alg-vals)]
                (float (/ (- v space) space))))
     rs)))

(defn percents [file]
  (let [info (utils/json-to-map file)
      ks   (remove #(=  % :iterations) (keys info))]
  (apply conj (map #(identity {% (percentage (% info))}) ks))))



(defn evaluate [file]
  (let [info (utils/json-to-map file)
        ks (keys info)
        key-range (map #(keyword (str %)) (range 1 65))]
    (map
     (fn [k]
       (let [data-key (k info)]
         (identity
          {k (map (fn [value-range]
                  (identity
                   {value-range (first
                                 (sort-by
                                  count >
                                  (map frequencies (value-range data-key))))}))
                key-range)})))
     ks)))

(defn pretty-spit
  [file-name collection]
  (spit (java.io.File. file-name)
        (with-out-str (pp/write collection :dispatch pp/code-dispatch))))

(def result-50 (percents multi-sat-50))
(def result-100 (percents multi-sat-100))
(def result-250 (percents multi-sat-250))
(def result-500 (percents multi-sat-500))
(def result-1000 (percents multi-sat-1000))
(def result-2500 (percents multi-sat-2500))
(def result-5000 (percents multi-sat-5000))

;; (let [ks (keys result-50)
;;       key-range (map #(keyword (str %)) (range 1 64))]
;;   #_(map #(identity {% (apply conj (% result-50))}) ks)
;;   (map (fn [k] (identity {k (let [data (apply conj (k result-50))]
;;                            (map #(vals (% data)) key-range)
;;                            )})) ks))


(defn to-json [output-file result]
  (spit (java.io.File. output-file)
        (json/write-str result)))

(def rs [50 100 250 500 1000 2500 5000])
(def results [result-50 result-100 result-250 result-500 result-1000 result-2500 result-5000])

(let [tmplt #(str "../statistics_charts/jsons/sat/salida-" % ".json")
      output-files (map tmplt rs)
      pairs (map (fn [a b] (vector a b)) output-files results)
      _ (println pairs)
      ]
  (run! #(to-json (first %) (second %)) pairs))




(spit (java.io.File. "data/salida-50.json")
      (with-out-str (pp/write (json/read-str (json/write-str (apply conj result-50))))))


(pretty-spit "data/salida-50.edn" result-50)
(pretty-spit "data/salida-100.edn" result-100)
(pretty-spit "data/salida-250.edn" result-250)
(pretty-spit "data/salida-500.edn" result-500)
(pretty-spit "data/salida-1000.edn" result-1000)
(pretty-spit "data/salida-2500.edn" result-2500)
(pretty-spit "data/salida-5000.edn" result-5000)

;; (key (first value-test))

;; (group-by #(get %) value-test)

#_(def processed-01 (utils/json-to-map file-01))
#_(def processed-02 (utils/json-to-map file-02))

#_(def last-processed (utils/json-to-map last-file))

(defn process [data]
  (let [ks (map #(keyword (str %)) (range 1 (inc (count data))))]
    (map #(identity {:core % :stats (utils/simple-stats (% data))}) ks)))

(defn extract [processed]
  (let [algorithms (keys processed)]
    (map #(identity {:algorithm % :stats (process (% processed))}) algorithms)))

(let [info (utils/json-to-map stats-sat-250)
      ks (keys info)
      fk (first ks)
      rks (rest ks)
      procs (range 1 65)
      processed (->> (map #(identity {% (map second (first (% info)))}) ks)
                     (apply conj))]
  (-> (c/xy-plot procs (fk processed)
                 :title "Brute SAT Solver"
                 :x-label "Processors"
                 :y-label "Time (nanoseconds)"
                 :legend true
                 :series-label (str fk)
                 :points true)
      (as-> chart
          (loop [k (first rks)
                 rks (rest rks)
                 chart chart]
            (if (and (nil? k) (empty? rks))
              chart
              (recur (first rks)
                     (rest rks)
                     (c/add-lines chart procs (k processed)
                                  :series-label (str k)
                                  :points true)))))
      (i/view)))

(defn plot-info
  [file-name operation]
  (let [processed (utils/json-to-map file-name)
        info (extract processed)
        procs (range 1 65)
        algorithms (keys processed)
        fk (first algorithms)
        rks (rest algorithms)
        ;; filtering (fn [info kw op] (->> info kw (map #(-> % op measurement))))
        filter-data (fn [info kw] (->> info
                                      (filter #(= (:algorithm %) kw))
                                      first
                                      :stats
                                      (map #(-> %
                                              :stats
                                              operation))))]
    (-> (c/xy-plot procs (filter-data info fk)
                   :title (str (name operation))
                   :x-label "Processors"
                   :y-label "Time (nanoseconds)"
                   :legend true
                   :series-label (str fk)
                   :points true)
        (as-> chart
            (loop [k (first rks)
                   rks (rest rks)
                   chart chart]
              (if (and (nil? k) (empty? rks))
                chart
                (recur (first rks)
                       (rest rks)
                       (c/add-lines chart procs
                                    (filter-data info k)
                                    :series-label (str k)
                                    :points true)))))
        (i/view))))

#_(plot-info last-file :mean)
