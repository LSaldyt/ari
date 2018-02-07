(ns ari.core
  "Translation script / effective test"
  (:require [clojure.tools.cli :refer [cli]]
            [ari.parse.parse :refer :all]
            [ari.parse.base :refer :all]
            [ari.metaparse.pybnf :refer [pybnf]]
            [ari.metaparse.ebnf :refer [ebnf]]
            [ari.translate :refer [translate]])
  (:gen-class))

(def test-separators ["=" " " ">>>" "\n"])
(def test-tag-pairs [[#"^[0-9]*$" "int"]])
(def test-parser (conseq [(wild :name) (token " ") (token "=" :op) (token " ") (tag "int" :value) (wild)]))

(def special-separators [["\"" "\"" :string] ["'" "'" :string] ["#" "\n" :comment]])

(defn -main [& in-args]
  (let [[opts args banner] (cli in-args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner))
    ;(println (pybnf "data/languages/simple.lang" "data/samples/test.simp"))
    ;(clojure.pprint/pprint (ebnf "data/languages/ebnf.lang"))
    ;(clojure.pprint/pprint (ebnf "data/languages/pascal_like.lang"))
    ;(println "EBNF/LISP tests")
    ;(let [lisp (ebnf "data/languages/lisp.lang")]
    ;  ;(println lisp)
    ;  (clojure.pprint/pprint (lisp "data/samples/simple_lisp.lisp")))
    (let [t (ebnf "data/languages/test.lang")]
      (clojure.pprint/pprint (t "data/samples/test.test")))))
    ;(let [[infile outfile] args]
    ;  (translate infile outfile test-parser test-separators test-tag-pairs))))
