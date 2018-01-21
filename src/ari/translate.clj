(ns ari.translate
  (:require [ari.metaparse :refer [metaparse]]
            [ari.parse :refer [parse]]
            [ari.lex :refer [lex]]))

(defn translate [[infile outfile] parser separators tag-pairs]
  (let [result 
    (->> (slurp infile)
         (lex separators tag-pairs)
         (parse parser))]
    (dorun (map #(println (str "\"" % "\"")) result))
    (spit outfile result)))
