(ns ari.translate
  (:require [ari.parse.parse :refer [parse]]
            [ari.lex :refer [lex]]))

(defn read-source [infile parser separators special-separators tag-pairs log]
  (println "Reading source")
  (println infile)
  (let [input (slurp infile)
        [lex-result in-log] (lex separators special-separators tag-pairs log input)
        [parse-result out-log] (parse parser in-log lex-result)]
    [parse-result out-log]))

(defn translate [infile outfile parser separators special-separators tag-pairs log]
  (let [[result log]
        (read-source infile parser separators special-separators tag-pairs log)]
    (spit outfile result)))
