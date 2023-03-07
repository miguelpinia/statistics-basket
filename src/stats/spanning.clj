(ns stats.spanning
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]
            [incanter.stats :as stats]))

(def torus2d-1000-true "data/spanning/st-TORUS_2D-true-1000000-1000.json")
(def torus2d-256-true "data/spanning/st-TORUS_2D-true-256-1000.json")

(defn spanning-operations [op file]
  (let [data (->> file
                  slurp
                  json/read-str
                  walk/keywordize-keys)
        rs   (range 0 64)
        execs (:executions data)
        algs (keys (:thread-0 execs))
        preprocessed (map #((keyword (str "thread-" %)) execs) rs)
        r (range 0 5)
        extract (fn [id op alg-result] (op ((keyword (str id)) (:data alg-result))))
        get-info (fn [alg range-vals op result] (identity {alg (stats/mean (map #(extract % op (alg result)) range-vals))}))]
    (map (fn [result]         (apply conj
                (map
                 #(get-info % r op result)
                 algs)))
       preprocessed)))

(spanning-operations :steals torus2d-1000-true)



(keyword "foo")
(map #(keyword (str "thread-" %)) (range 1 65))
