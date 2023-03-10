(ns stats.eval
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [incanter.core :as i]
            [incanter.io :as iio]
            [incanter.stats :as s]
            [incanter.charts :as c]
            [incanter.distributions :as d]
            [clojure.walk :refer [keywordize-keys]])
  (:import (java.io EOFException)))

(defn load-json [file-name]
  (->> file-name
       slurp
       json/read-str
       keywordize-keys
       (conj [])
       i/to-dataset))

(defn get-data [data column-name] (map #(column-name %) data))

(defn to-indexed [row] (keep-indexed #(identity [%1 %2]) row))

(defn get-data-by-col [data col]
  (->> (get-data data col)
       (map #(to-indexed %))
       (apply concat)
       (group-by #(first %))
       (map  (fn [proc-data]
             (identity [(first proc-data)
                        (into [] (map #(second %) (second proc-data)))])))
       (into {})))

;; (get-data-by-col data-3 :CAS)

(defn statistics [data column]
  (let [info (get-data-by-col data column)]
    (for [[proc measurements] info]
      (identity [proc {:mean (s/mean measurements)
                       :sd (s/sd measurements)
                       :variance (s/variance measurements)
                       :median (s/median measurements)
                       :ci (s/simple-ci measurements)}]))))

(defn get-mean [data column]
  (let [stats (statistics data column)]
    (->> stats
         (map #(identity [(first %) (:mean (second %))]))
         (sort-by first)
         (map second))))

(defn get-median [data column]
  (let [stats (statistics data column)]
    (->> stats
         (map #(identity [(first %) (:median (second %))]))
         (sort-by first)
         (map second))))

(defn get-ci [data column]
  (let [stats (statistics data column)]
    (->> stats
         (map #(identity [(first %) (:ci (second %))]))
         (sort-by first)
         (map second))))

;; (def sequence (range 65 128))

(defn filter-by-8 [sequence]
  (let [multiple-8 #(= (mod % 8) 0)]
    (->> sequence
         (map #(vector %1 %2) (range))
         (filter #(multiple-8 (inc (first %))))
         (map second))))

;; (filter-by-8 sequence)

(defn plot-means
  ([data title]
   (plot-means data title (range 1 65)))
  ([data title procs]
   (let [
         mean-cas    (get-mean data :CAS)
         mean-cas-t  (get-mean data :CAST)
         mean-fai    (get-mean data :FAIDELAY)
         mean-rwnc   (get-mean data :RWNC)
         mean-rwnc-t (get-mean data :RWNCT)
         mean-rwsqrt (get-mean data :RWSQRT)]
     (-> (c/xy-plot procs mean-cas
                    :title title
                    :x-label "Processors"
                    :y-label "Time (nanoseconds)"
                    :legend true
                    :series-label "Mean CAS"
                    :points true)
         (c/add-lines procs mean-fai
                      :series-label "Mean FAI"
                      :points true)
         (c/add-lines procs mean-cas-t
                      :series-label "Mean CAS-T"
                      :points true)
         (c/add-lines procs mean-rwnc
                      :series-label "Mean RWNC"
                      :points true)
         (c/add-lines procs mean-rwnc-t
                      :series-label "Mean RWNC-T"
                      :points true)
         (c/add-lines procs mean-rwsqrt
                      :series-label "Mean RW SQRT"
                      :points true)
         #_(i/save (str title ".png"))
         (i/view)))))



;; 5,000,000 operations
#_(do
  (def json-test-1 "data/2023-02-13-20:28:37_experiment_time_execution.json")
  (def d-1 (load-json json-test-1))
  (def column-names-1 (map #(keyword (str "iter-" %)) (range (i/$ :iterations d-1))))
  (def data-1 (map #(->> d-1 (i/$ %)) column-names-1))
  (plot-means data-1 "Statistics 5'000'000 operations"))

#_(do
  (def json-test-2 "data/2023-02-14-20:37:42_experiment_time_execution.json")
  (def d-2 (load-json json-test-2))
  (def column-names-2 (map #(keyword (str "iter-" %)) (range (i/$ :iterations d-2))))
  (def data-2 (map #(->> d-2 (i/$ %)) column-names-2))
  (plot-means data-2 "Statistics 20'000'000 operations"))

#_(do
  (def json-test-3 "data/2023-02-16-15:14:02_experiment_time_execution.json")
  (def d-3 (load-json json-test-3))
  (def column-names-3 (map #(keyword (str "iter-" %)) (range (i/$ :iterations d-3))))
  (def data-3 (map #(->> d-3 (i/$ %)) column-names-3))
  (plot-means data-3 "Statistics 100'000'000 operations"))

#_(do
  (def json-test-4 "data/2023-02-22-19:54:06_experiment_time_execution.json")
  (def d-4 (load-json json-test-4))
  (def column-names-4 (map #(keyword (str "iter-" %)) (range (i/$ :iterations d-4))))
  (def data-4 (map #(->> d-4 (i/$ %)) column-names-4))
  (plot-means data-4 "Statistics 20'000'000 operations"))


#_(do
  (def json-test-5 "data/2023-02-23-11:03:10_experiment_time_execution.json")
  (def d-5 (load-json json-test-5))
  (def column-names-5 (map #(keyword (str "iter-" %)) (range (i/$ :iterations d-5))))
  (def data-5 (map #(i/$ % d-5) column-names-5))
  (plot-means data-5 "Statistics 20'000'000 operations"))


(defn mean [xs]
  (/ (apply + xs)
     (count xs)))

;; (mean (get (get-data-by-col data-3 :CAS) 0))

(defn variance [xs]
  (let [m (mean xs)
        square-error (fn [x]
                       (Math/pow (- x m) 2))]
    (mean (map square-error xs))))

(defn standard-deviation [xs]
  (Math/sqrt (variance xs)))

(defn standard-error [xs]
  (/ (standard-deviation xs)
     (Math/sqrt (count xs))))

(defn sq [x]
  (Math/pow x 2))

(defn confidence-interval [p xs]
  (let [x-bar  (s/mean xs)
        se     (standard-error xs)
        z-crit (s/quantile-normal (- 1 (/ (- 1 p) 2)))]
    [(- x-bar (* se z-crit))
     (+ x-bar (* se z-crit))]))

(defn pooled-standard-deviation [a b]
  (i/sqrt (+ (i/sq (standard-deviation a))
             (i/sq (standard-deviation b)))))

;; (defn pooled-standard-error [a b]
;;   (i/sqrt (+ (/ (i/sq (standard-
;;   deviation a)) (count a))
;;              (/ (i/sq (standard-deviation b)) (count b)))))

(defn pooled-standard-error [a b]
  (i/sqrt (+ (i/sq (standard-error a))
             (i/sq (standard-error b)))))

(defn z-stat [a b]
  (-> (- (mean a)
         (mean b))
      (/ (pooled-standard-error a b))))

;; One-tailed test
(defn z-test [a b]
  (s/cdf-normal (z-stat a b)))

#_(defn z-test [a b]
  (- 1 (s/cdf-normal (i/abs (z-stat a b)))))

(def t-stat z-stat)

(defn t-test [a b]
  (let [df (+ (count a) (count b) -2)]
    - (1 s/cdf-t ((i/abs (t-stat a b)) :df df))))

(defn t-test-2-tails [a b]
  (let [df (+ (count a) (count b) -2)]
    (s/cdf-normal (- 1 (/ (- 0 1.64) 2)))))

(defn sst [groups]
  (->> (apply concat groups)
       (s/sum-of-square-devs-from-mean)))

(defn ssw [groups]
  (->> (map s/sum-of-square-devs-from-mean groups)
       (reduce +)))

(defn ssb [groups]
  (- (sst groups)
     (ssw groups)))

(defn f-stat [groups df1 df2]
  (let [msb (/ (ssb groups) df1)
        msw (/ (ssw groups) df2)]
    (/ msb msw)))

;; (s/cdf-f 3456 :df1 )

(defn f-test [groups]
  (let [n (count (apply concat groups))
        m (count groups)
        df1 (- m 1)
        df2 (- n m)
        f-stat (f-stat groups df1 df2)]
    (s/cdf-f f-stat :df1 df1 :df2 df2 :lower-tail? false)))

(def last-file "data/2023-03-01-22:29:34_experiment_time_execution.json")
(def last-file-40 "data/2023-03-02-00:42:46_experiment_time_execution.json")
(def last-file-40-wm "data/2023-03-02-09:07:52_experiment_time_execution.json")

(defn group-data [data proc-num]
  (let [normalize (fn [xs] (map #(/ % 1000000) xs))]
    (map (comp normalize
           #(get % proc-num)
           first
           vals)
     data)))

;; (defn f-test-proc [ds proc]
;;   (let [ks      (keys (first ds))
;;         data    (for [k ks]
;;                   (identity {k (get-data-by-col ds k)}))
;;         grouped (group-data data proc)
;;         n       (count (apply concat grouped))]
;;   #_(f-stat grouped (count ap))
;;   n))

(defn get-f-test [file proc-num]
  (let [json      (load-json file)
        c-names   (map #(keyword (str "iter-" %)) (range (i/$ :iterations json)))
        data      (map #(i/$ % json) c-names)
        get-by    (fn [coll kw idx] (->> coll (map kw) (map #(get % idx))))
        processed (->> data
                       (first)
                       (keys)
                       (map #(identity [% (get-by data % proc-num)]))
                       (into {})
                       (vals))
        n         (count (apply concat processed))
        m         (count processed)
        df1       (- m 1)
        df2       (- n m)]
    {:f-test (format "%.32f" (f-test processed))
     :df1 df1
     :df2 df2}))


(defn get-t-test [file proc-num & {:keys [group-1 group-2]}]
  (let [json      (load-json file)
        c-names   (map #(keyword (str "iter-" %)) (range (i/$ :iterations json)))
        data      (map #(i/$ % json) c-names)
        get-by    (fn [coll kw idx] (->> coll (map kw) (map #(get % idx))))
        processed (->> data
                       (first)
                       (keys)
                       (map #(identity [% (get-by data % proc-num)]))
                       (into {}))
        g-1       (group-1 processed)
        g-2       (group-2 processed)]
  (s/t-test g-1 :y g-2)))


;; Dibujamos la gr??fica de los datos
(let [file    last-file-40-wm
      json    (load-json file)
      c-names (map #(keyword (str "iter-" %)) (range (i/$ :iterations json)))
      data    (map #(i/$ % json) c-names)]
  (plot-means data "Statistics 10'000'000 operations"))

;; Calcula f-test sobre todos los grupos indicando la cantidad de
;; procesadores utilizada para la prueba.
#_(let [file     last-file-40-wm
      proc-num 50]
  (get-f-test last-file proc-num))


;; Calcula la t-test sobre dos grupos, indicando la cantida de
;; procesadores utilizados para la prueba. En este ejemplo, queremos
;; comprobar si, para la versi??n CAS y la version RWNC hay una
;; diferencia entre ambos. La hip??tesis nula es que ambos procesos son
;; estad??sticamente iguales. La hip??tesis alternativa es que hay
;; diferencias estadisticamente significativas. Dado el p-value y el
;; t-score para este ejemplo, podemos rechazar nuestra hipotesis nula.
#_(let [file last-file-40-wm
      proc-num 51
      group-1 :FAIDELAY
      group-2 :RWSQRT]
  (get-t-test file proc-num :group-1 group-1 :group-2 group-2))



#_(let [json      (load-json last-file-40-wm)
      c-names   (map #(keyword (str "iter-" %)) (range (i/$ :iterations json)))
      processed (map #(->> json (i/$ %)) c-names)
      ks        (keys (first processed))
      _         (print ks)
      data      (for [k ks]
                  {k (get-data-by-col processed k)})
      grouped   (group-data data 51)
      box-plot  (c/box-plot (first grouped)
                            :x-label "Operation type"
                            :y-label "Time (s)")
      add-box   (fn [chart times]
                  (c/add-box-plot chart times))]
    (-> (reduce add-box box-plot (rest grouped))
        (i/view)))


#_(let [json     (load-json "data/2023-02-24-13:01:21_experiment_time_execution.json")
      ks       (keys (first json))
      data     (for [k ks]
                 (identity {k (get-data-by-col json k)}))
      grouped  (group-data data 0)
      box-plot (c/box-plot (first grouped)
                           :x-label "Operation type"
                           :y-label "Time (ns)")
      add-box  (fn [chart times]
                 (c/add-box-plot chart times))]
  (-> (reduce add-box box-plot (rest grouped))
      (i/view)))

#_(let [ks (keys (first data-4))
      data (for [k ks]
             (identity {k (get-data-by-col data-4 k)}))
      grouped (group-data data)
      box-plot (c/box-plot (first grouped)
                           :x-label "Operation type"
                           :y-label "Time (ns)")
      add-box (fn [chart times]
                (c/add-box-plot chart times))]
  (-> (reduce add-box box-plot (rest grouped))
      (i/view)))


#_(let [data-zero-cas   (get (get-data-by-col data-4 :CAS) 0)
        data-zero-cas-t (get (get-data-by-col data-4 :CAST) 0)
        ;; m (mean data-zero)
        ;; v (variance data-zero)
        ;; sd (standard-deviation data-zero)
        ;; ci (confidence-interval 0.05 data-zero)
        ;; psd (pooled-standard-deviation data-zero-cas data-zero-cas-t)
        ;; pse (pooled-standard-error data-zero-cas data-zero-cas-t)
        tstat (t-stat data-zero-cas data-zero-cas-t)
        ;; df (+ (count data-zero-cas) (count data-zero-cas-t) -2)
        ttest (t-test data-zero-cas data-zero-cas-t)
        ]
    (println tstat ", " ttest))

#_(let [ks (keys (first data-4))
        data (for [k ks]
               (identity {k (get-data-by-col data-4 k)}))
        grouped (group-data data 2)
        n (count (apply concat grouped))]
    (f-test grouped))

;; (f-test-proc data-4 0)
