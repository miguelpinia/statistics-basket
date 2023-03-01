(defproject statistics_basket "0.1.0-SNAPSHOT"
  :description    "Clojure project for generate statistics and charts for the analysis of data from basket queues project"
  :url            "https://github.com/miguelpinia/statistics-basket"
  :license        {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
                   :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies   [[org.clojure/clojure "1.11.1"]
                   [incanter "1.9.3"]]
  :resource-paths ["data"]
  :main           ^:skip-aot stats.core
  :target-path    "target/%s"
  :repl-options   {:init-ns stats.eval}
  :profiles       {:uberjar {:aot      :all
                             :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :jvm-opts       ["-Xmx2G"])
