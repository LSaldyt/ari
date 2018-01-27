(ns ari.core
  "Translation script / effective test"
  (:require [clojure.tools.cli :refer [cli]]
            [ari.parse     :refer :all]
            [ari.metaparse.pybnf :refer [pybnf]]
            [ari.metaparse.ebnf :refer [ebnf]]
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
    ;;(clojure.pprint/pprint (pybnf "data/languages/simple.lang" "data/test_simple.simp"))
    (clojure.pprint/pprint (ebnf "data/languages/ebnf_test.lang"))
    (clojure.pprint/pprint (ebnf "data/languages/ebnf.lang"))))
    ;(let [[infile outfile] args]
    ;  (translate infile outfile test-parser test-separators test-tag-pairs))))
