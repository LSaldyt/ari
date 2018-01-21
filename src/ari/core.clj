(ns ari.core
  (:require [clojure.tools.cli :refer [cli]]
            [ari.parse     :refer :all]
            [ari.metaparse :refer [metaparse]]
            [ari.translate :refer [translate]])
  (:gen-class))

(def test-separators ["=" " " ">>>" "\n"])
(def test-tag-pairs [[#"^[0-9]*$" "int"]])
(def test-parser (inorder [(wild :name) (token " ") (token "=" :op) (token " ") (tag "int" :value) (wild)]))
; 
; (def test-many (inorder [(many (token ".")) (token "!")]))
; 
; (def test-any-of (any-of [(token "!") (token ".")]))
; 
; (defn parse [content]
;   (println (test-many [["." ""] ["." ""] ["!" ""]]))
;   (println (test-any-of [["." ""]]))
;   (println (test-any-of [["!" ""]]))
;   (println content)
;   (println (test-assignment content))
;   content)

(defn -main [& in-args]
  (let [[opts args banner] (cli in-args
    ["-h" "--help" "Print this help"
     :default false :flag true])]
    (when (:help opts)
      (println banner))
    (translate args test-parser test-separators test-tag-pairs)))

; (defn translate [[infile outfile] parser separators tag-pairs]
