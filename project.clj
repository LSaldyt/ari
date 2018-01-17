(defproject runs "0.1.0"
  :description "Multi-language parser"
  :url "github.com/LSaldyt/ari"
  :license {:name "GNU Public License"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.4"]]
  :plugins [[lein-bin "0.3.4"]]
  :bin { :name "ari" }
  :main ari.core)
