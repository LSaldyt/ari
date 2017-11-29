(ns app.core
  (:require [clojure.tools.cli :refer [cli]]
            [clojure.data.json :as json])
  (:gen-class))

(defn transform [from]
  {"test" "a"})

(defn process-json []
  (let [input (json/read-str (slurp "in.json"))]
  (spit "out.json" (json/write-str (transform input)))))

(defn -main [& args]
  (let [[opts args banner] (cli args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner)))
  (process-json))
