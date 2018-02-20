(ns ari.core_test
  (:require [clojure.test :refer :all]
            [ari.core :refer :all]
            [ari.parse.parse :refer :all]
            [ari.parse.base :refer :all]
            [ari.metaparse.pybnf :refer [pybnf]]
            [ari.metaparse.ebnf :refer [ebnf]]
            [ari.translate :refer [translate]]))

(defn tlang [filename]
  (str "test/data/languages/" filename))

(defn tsample [filename]
  (str "test/data/samples/" filename))

(defn test-pybnf [in out expected]
  (let [result (pybnf (tlang in) (tsample out))
        tree   (first (first result))]
    ;(println tree)
    (= tree expected)))

(deftest pybnf-test
  (testing "simple"
    (test-pybnf "simple.lang" "test.simp" {:values '({:n {:token ["b" "unknown"]}})})
    (test-pybnf "lisp.pybnf" "pylisp" {:values '({:token ["\n" "unknown"]})})))

; (let [list-lang (ebnf "data/languages/list.lang")]
;   (clojure.pprint/pprint (list-lang "data/samples/list")))
;  (clojure.pprint/pprint (pybnf "data/languages/lisp.pybnf" "data/samples/simple_list2.lisp"))
