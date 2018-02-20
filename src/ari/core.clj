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
    (do-pybnf "simple.lang" "test.simp")
    (do-pybnf "lisp.pybnf" "pylisp")
    ; (clojure.pprint/pprint (ebnf "data/languages/ebnf.lang"))
    ; (clojure.pprint/pprint (ebnf "data/languages/pascal_like.lang"))
    ; (println "EBNF/LISP tests")
    ; (let [t (ebnf "data/languages/t")]
    ;   (clojure.pprint/pprint (t "data/samples/t")))
    ; (let [t2 (ebnf "data/languages/t2")]
    ;   (clojure.pprint/pprint (t2 "data/samples/t2")))
    ;(println (ebnf "data/languages/C.lang"))
    ;(/ 1 0)
     ;(let [lisp (ebnf "data/languages/lisp.lang")]
     ;  ;(println lisp)
     ;  (clojure.pprint/pprint (lisp "data/samples/simple_lisp.lisp"))
     ;  (clojure.pprint/pprint (lisp "data/samples/simple_lisp2.lisp")))
    ;(let [list-lang (ebnf "data/languages/list.lang")]
    ;  (clojure.pprint/pprint (list-lang "data/samples/list")))
    ; (clojure.pprint/pprint (pybnf "data/languages/lisp.pybnf" "data/samples/simple_list2.lisp"))
    ; (let [t (ebnf "data/languages/test.lang")]
    ;   (clojure.pprint/pprint (t "data/samples/test.test")))
    ;(let [[infile outfile] args]
    ;  (translate infile outfile test-parser test-separators test-tag-pairs))))
    ))
