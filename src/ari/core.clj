(ns ari.core
  (:require [clojure.tools.cli :refer [cli]]
            [ari.parse :refer [parse]]
            [ari.lex :refer [lex]])
  (:gen-class))

(defn translate [[infile outfile]]
  (let [result 
    (->> (slurp infile)
         (lex)
         (parse))]
    (dorun (map #(println (str "\"" % "\"")) result))
    (spit outfile result)))

(defn -main [& in-args]
  (let [[opts args banner] (cli in-args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner))
    (translate args)))
