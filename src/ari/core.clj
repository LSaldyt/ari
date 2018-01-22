(ns ari.core
  (:require [clojure.tools.cli :refer [cli]]
            [ari.parse     :refer :all]
            [ari.metaparse :refer [metaparse]]
            [ari.translate :refer [translate]])
  (:gen-class))

(def test-separators ["=" " " ">>>" "\n"])
(def test-tag-pairs [[#"^[0-9]*$" "int"]])
(def test-parser (psequence [(wild :name) (token " ") (token "=" :op) (token " ") (tag "int" :value) (wild)]))

(defn -main [& in-args]
  (let [[opts args banner] (cli in-args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner))
    (clojure.pprint/pprint (metaparse "data/languages/simple.lang"))
    (let [[infile outfile] args]
      (translate infile outfile test-parser test-separators test-tag-pairs))))
