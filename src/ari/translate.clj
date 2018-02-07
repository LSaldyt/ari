(ns ari.translate
  (:require [ari.parse :refer [parse]]
            [ari.lex :refer [lex]]))

(defn read-source [infile parser separators special-separators tag-pairs]
  (println "Reading source")
  (println infile)
  ; (println parser)
  ; (println separators)
  ; (println special-separators)
  ; (println tag-pairs)
  (->> (slurp infile)
       (lex separators special-separators tag-pairs)
       (parse parser)))

(defn translate [infile outfile parser separators special-separators tag-pairs]
  (let [result (read-source infile parser separators special-separators tag-pairs)]
    (spit outfile result)))
