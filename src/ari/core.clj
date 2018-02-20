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

(defn lang [filename]
  (str "data/languages/" filename))

(defn sample [filename]
  (str "data/samples/" filename))

(defn do-pybnf [in out]
  (clojure.pprint/pprint (pybnf (lang in) (sample out))))

(defn -main [& in-args]
  (let [[opts args banner] (cli in-args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner))
     (let [lisp (ebnf "data/languages/lisp.lang")]
       (clojure.pprint/pprint (lisp "data/samples/simple_lisp.lisp")))
    ;  (translate infile outfile test-parser test-separators test-tag-pairs))))
    ))
