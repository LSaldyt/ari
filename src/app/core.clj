(ns app.core
  (:require [clojure.tools.cli :refer [cli]]
            [clojure.data.json :as json])
  (:gen-class))

(defn transform [from]
  (assoc from :key "added"))

(defn process-json [[infile outfile]]
  (let [input (json/read-str (slurp infile))]
  (spit outfile (json/write-str (transform input)))))

(defn -main [& in-args]
  (let [[opts args banner] (cli in-args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner))
    (process-json args)))
