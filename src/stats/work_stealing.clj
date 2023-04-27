(ns stats.work-stealing
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]
            ;; [clojure.pprint :as pp]
            [incanter.stats :as s]
            [incanter.charts :as c]
            [incanter.core :as i]))

(def file "data/experiment-01.json")

(defn json-to-map [file-name]
  (->> file-name
       slurp
       json/read-str
       walk/keywordize-keys))

(def result (->> file
                 slurp
                 json/read-str
                 walk/keywordize-keys))

(defn simple-stats [data]
  {:ci       (s/simple-ci data)
   :mean     (s/mean data)
   :sd       (s/sd data)
   :variance (s/variance data)
   :median   (s/median data)})

(defn process-stats-alg [alg thread info]
  (let [info   (-> (thread info) alg :data)
        size   (count info)
        ks     (map #(keyword (str %)) (range size))
        pts    (map #(select-keys (identity (% info))  [:puts :steals :takes]) ks)
        puts   (map :puts pts)
        steals (map :steals pts)
        takes  (map :takes pts)]
    {alg {:puts   (simple-stats puts)
          :steals (simple-stats steals)
          :takes  (simple-stats takes)}}))

(defn process-stats [data]
  (let [processors (:processors data)
        iters      (map #(keyword (str "thread-" %)) (range processors))
        algorithms (map keyword (:algorithms data))
        info       (:executions data)
        proccess   (fn [thread] (into {} (map #(process-stats-alg % thread info) algorithms)))]
    (map proccess iters)))

(defn process-to-plot [data]
  (let [processed  (process-stats data)
        algorithms (map keyword (:algorithms data))
        transform  (fn [alg] {alg (map alg processed)})]
    (into {} (map transform algorithms))))

(defn plot-info [file-name operation measurement]
  (let [json       (json-to-map file-name)
        graph      (:graphType json)
        directed   (if (:directed json) "Directed" "No-Directed")
        info       (process-to-plot json)
        procs      (range 1 65)
        algorithms (map keyword (:algorithms result))
        fk         (first algorithms)
        rks        (rest algorithms)
        filtering  (fn [info kw op] (->> info kw (map #(-> % op measurement))))]
    (-> (c/xy-plot procs (filtering info fk operation)
                   :title (str (name operation) " - " (name graph) " " directed
                               " (" (name measurement) ")")
                   :x-label "Processors"
                   :y-label "Operations performed"
                   :legend true
                   :series-label (str fk)
                   :points true)
        (as-> chart
              (loop [k     (first rks)
                     rks   (rest rks)
                     chart chart]
                (if (and (nil? k) (empty? rks))
                  chart
                  (recur (first rks)
                         (rest rks)
                         (c/add-lines chart procs
                                      (filtering info k operation)
                                      :series-label (str k)
                                      :points true)))))
        (i/view))))

(plot-info "data/experiment-03.json" :steals :mean)
