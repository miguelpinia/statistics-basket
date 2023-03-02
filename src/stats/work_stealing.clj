(ns stats.work-stealing
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]
            [incanter.stats :as s]
            [incanter.charts :as c]
            [incanter.core :as i]
            [incanter.pdf :as p]
            [stats.utils :as utils]))

(def file "data/experiment-01.json")

(defn json-to-map [file]
  (->> file
       slurp
       json/read-str
       walk/keywordize-keys))

(def result (json-to-map file))

(defn process-stats-alg [alg thread info]
  (let [info   (-> (thread info) alg :data)
        size   (count info)
        ks     (map #(keyword (str %)) (range size))
        pts    (map #(select-keys (identity (% info))  [:puts :steals :takes :executionTime]) ks)
        puts   (map :puts pts)
        steals (map :steals pts)
        takes  (map :takes pts)
        time   (map :executionTime pts)]
    {alg {:puts   (utils/simple-stats puts)
          :steals (utils/simple-stats steals)
          :takes  (utils/simple-stats takes)
          :time   (utils/simple-stats time)}}))

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

(defn plot-info
  "Genera la gráfica para los resultados de work-stealing. Recibe el
  nombre del archivo file-name, la operación a
  evaluar (:puts :steals :takes :time) y el estadístico a
  representar (:mean :sd :variance :median)"
  [file-name operation measurement]
  (let [json       (utils/json-to-map file-name)
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
                   :y-label (if (= operation :time) "Time (nanoseconds)" "Total Operations")
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
        (i/view)
        )))

#_(plot-info "data/experiment-05.json" :time :mean)
;;st-RANDOM-1000000-1000000-undirected-2.json
(def results
  {:torus-2d-directed-256     {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS-2D-1000-256-directed.json"
                               :graph       "Torus 2D directed 256 items"
                               :output-file "torus-2d-directed-256"}
   :torus-2d-directed-1m      {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_2D-1000-1000000-directed.json"
                               :graph       "Torus 2D directed 1 million items"
                               :output-file "torus-2d-directed-1m"}
   :torus-2d-undirected-256   {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_2D-1000-256-undirected.json"
                               :graph       "Torus 2D undirected 256 items"
                               :output-file "torus-2d-undirected-256"}
   :torus-2d-undirected-1m    {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_2D-1000-1000000-undirected.json"
                               :graph       "Torus 2D undirected 1 million items"
                               :output-file "torus-2d-undirected-1m"}
   :torus-2d60-directed-256   {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_2D_60-1000-256-directed.json"
                               :graph       "Torus 2D60 directed 256 items"
                               :output-file "torus-2d60-directed-256"}
   :torus-2d60-directed-1m    {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_2D_60-1000-1000000-directed.json"
                               :graph       "Torus 2D60 directed 1 million items"
                               :output-file "torus-2d60-directed-1m"}
   :torus-2d60-undirected-256 {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_2D_60-1000-256-undirected.json"
                               :graph       "Torus 2D60 undirected 256 items"
                               :output-file "torus-2d60-undirected-256"}
   :torus-2d60-undirected-1m  {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_2D_60-1000-1000000-undirected.json"
                               :graph       "Torus 2D60 undirected 1 million items"
                               :output-file "torus-2d60-undirected-1m"}
   :torus-3d-directed-256     {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_3D-100-256-directed.json"
                               :graph       "Torus 3D directed 256 items"
                               :output-file "torus-3d-directed-256"}
   :torus-3d-directed-1m      {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_3D-100-1000000-directed.json"
                               :graph       "Torus 3D directed 1 million items"
                               :output-file "torus-3d-directed-1m"}
   :torus-3d-undirected-256   {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_3D-100-256-undirected.json"
                               :graph       "Torus 3D undirected 256 items"
                               :output-file "torus-3d-undirected-256"}
   :torus-3d-undirected-1m    {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_3D-100-1000000-undirected.json"
                               :graph       "Torus 3D undirected 1 million items"
                               :output-file "torus-3d-undirected-1m"}
   :torus-3d40-directed-256   {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_3D_40-100-256-directed.json"
                               :graph       "Torus 3D40 directed 256 items"
                               :output-file "torus-3d40-directed-256"}
   :torus-3d40-directed-1m    {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_3D_40-100-1000000-directed.json"
                               :graph       "Torus 3D40 directed 1 million items"
                               :output-file "torus-3d40-directed-1m"}
   :torus-3d40-undirected-256 {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_3D_40-100-256-undirected.json"
                               :graph       "Torus 3D40 undirected 256 items"
                               :output-file "torus-3d40-undirected-256"}
   :torus-3d40-undirected-1m  {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-TORUS_3D_40-100-1000000-undirected.json"
                               :graph       "Torus 3D40 undirected 1 million items"
                               :output-file "torus-3d40-undirected-1m"}
   :random-directed-256       {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-RANDOM-1000000-256-directed.json"
                               :graph       "Random directed 256 items"
                               :output-file "random-directed-256"}
   :random-directed-1m        {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-RANDOM-1000000-1000000-directed.json"
                               :graph       "Random directed 1 million items"
                               :output-file "random-directed-1m"}
   :random-undirected-256     {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-RANDOM-1000000-256-undirected.json"
                               :graph       "Random undirected 256 items"
                               :output-file "random-undirected-256"}
   :random-undirected-1m      {:file        "../statistics_charts/jsons/last/graph/multiplicity/multiplicity/st-RANDOM-1000000-1000000-undirected.json"
                               :graph       "Random undirected 1 million items"
                               :output-file "random-undirected-1m"}})



(defn process [arr]
  (let [simplified  (reduce
                     (fn [acc obj]
                       (assoc acc :puts (concat (:puts acc) (:puts obj))
                              :takes (concat (:takes acc) (:takes obj))
                              :steals (concat (:steals acc) (:steals obj))))
                     arr)
        puts        (:puts simplified)
        takes       (:takes simplified)
        steals      (:steals simplified)
        mean-puts   (s/mean puts)
        sd-puts     (s/sd puts)
        mean-takes  (s/mean takes)
        sd-takes    (s/sd takes)
        mean-steals (s/mean steals)
        sd-steals   (s/sd steals)]
    (identity {:puts   {:mean               mean-puts
                        :standard-deviation sd-puts
                        :sd-percent         (if (> mean-puts 0) (* (/ sd-puts mean-puts) 100) 0)
                        :variance           (s/variance puts)
                        :median             (s/median puts)
                        :ci                 (s/simple-ci puts)}
               :takes  {:mean               mean-takes
                        :standard-deviation sd-takes
                        :sd-percent         (if (> mean-takes 0) (* (/ sd-takes mean-takes) 100) 0)
                        :variance           (s/variance takes)
                        :median             (s/median takes)
                        :ci                 (s/simple-ci takes)}
               :steals {:mean               mean-steals
                        :standard-deviation sd-steals
                        :sd-percent         (if (> mean-steals 0) (* (/ sd-steals mean-steals) 100) 0)
                        :variance           (s/variance steals)
                        :median             (s/median steals)
                        :ci                 (s/simple-ci steals)}
               :work (- mean-puts mean-takes)})))


(defn process-entries [entries]
  (let [idxs (map #(keyword (str %)) (range 64))]
    (map
     (fn [idx] (-> (idx entries)
                  process))
     idxs)))

(defn process-multiplicity [execs algs]
  (map (fn [alg] (-> execs
                  alg
                  (as-> values (identity {alg (process-entries values)}))
                  (as-> values (apply conj values))))
       algs))

(defn pretty-spit [file-name collection]
  (spit (java.io.File. file-name)
        (with-out-str (pp/write collection :dispatch pp/code-dispatch))))

(defn to-json [output-file result]
  (spit (java.io.File. output-file)
        (with-out-str (pp/write (json/read-str (json/write-str (apply conj result) ))))
        ))

#_(let [execs (-> results :torus-2d-directed-256 :file json-to-map :executions)
      ks  (keys execs)
      result-60 (into {} (map (fn [k] (identity {k (-> execs k :60)})) ks))]
  (map (fn [k] (map (fn [data] (:puts data))(-> result-60 k))) ks))

(defn sum-measurements [entry]
  {:puts   (reduce +  (:puts entry))
   :takes  (reduce + (:takes entry))
   :steals (reduce + (:steals entry))})

(defn mean-by-type [entries type]
  (->> entries
       (map type)
       (s/mean)))

(defn mean-entries [entries]
  (let [mean-puts  (mean-by-type entries :puts)
        mean-takes (mean-by-type entries :takes)]
    {:puts       mean-puts
     :takes      mean-takes
     :difference (-> (- mean-puts mean-takes)
                     (/ mean-puts)
                     (* 100))
     :excedent   (-> (- mean-puts 1000000)
                    (/ mean-puts)
                    (* 100))
     :executed   (-> (- mean-takes 1000000)
                   (/ mean-takes)
                   (* 100))}))



(defn means [processor data]
  (->> (processor data)
       (map sum-measurements)
       mean-entries))

(defn process-puts-minus-takes
  [execution]
  (let [ks    (keys execution)
        procs (map #(keyword (str %)) (range 0 64))]
    (into {} (map (fn [k]
                 (let [data (k execution)
                       ms #(means % data)]

                   {k (map ms procs)})) ks))))

(defn get-execution [results type]
  (-> results type :file json-to-map :executions))

(def execution (get-execution results :torus-2d-directed-256))

#_(process-puts-minus-takes execution)

(def multiplicity-example (json-to-map (-> results :torus-2d-directed-256 :file)))

(defn plot-multiplicity [execs graph]
  (let [ks        (keys execs)
        fk        (first ks)
        rks       (rest ks)
        procs     (range 1 65)
        processed (into {} (process-multiplicity execs ks))
        extract   (fn [values] (map #(-> % :work) values))]
    ;; processed
    (-> (c/xy-plot procs (-> processed fk extract)
                   :title (str "Puts - Takes (average)" graph)
                   :x-label "Processors"
                   :y-label "Puts - Takes"
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
                       (c/add-lines chart procs (-> processed k extract)
                                    :series-label (str k)
                                    :points true)))))
        (i/view)
        #_(p/save-pdf (str "../statistics_charts/jsons/output/" graph ".pdf") :width 1920 :height 1080))
    (pretty-spit (str "../statistics_charts/jsons/output/" graph ".edn") processed)))


#_(let [ks (keys results)]
  (pmap (fn [algorithm]
          (-> results
              algorithm
              :file
              json-to-map
              (as-> execs
                    (into {} (process-multiplicity execs (keys execs))))))
        ks))

(let [ks (keys results)]
   (pmap #(-> results
           (get-execution %)
           process-puts-minus-takes
           json/write-str
           (as-> content (spit (str "../statistics_charts/jsons/output/multiplicity/" (:output-file (% results)) ".json")
                               content)))
      ks))


#_(let [ks (keys results)]
  (pmap #(-> results
             (get-execution %)
             process-puts-minus-takes
             json/write-str
             (as-> content (spit (str (:output-file (% results)) ".json")
                                 content))

             plot-multiplicity (-> results
                                   %
                                   :file
                                   json-to-map
                                   :executions)
                            (-> results
                                %
                                :graph
                                ))
        ks))
