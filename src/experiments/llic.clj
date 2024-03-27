(ns experiments.llic
  (:require [experiments.analysis-queue :as q]
            [experiments.utils :as u]
            [clojure.java.io :as io]))

#_(q/analysis "extract_insert"
            64
            "data/queue/inner/64/analysis.json"
            "data/queue/inner/64/")

#_(q/analysis "enq_deq"
            64
            "data/queue/enq_deq/64/analysis.json"
            "data/queue/enq_deq/64")
