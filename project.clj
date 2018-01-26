(defproject ari "0.1.0"
  :description 
"Ari is a continued investigation into computational linguistics.
Basically, ari is a framework for doing programming language research. 
Right now, it is a parser-generator, but eventually it will become a capable translator, based on glossa (github.com/LSaldyt/glossa)"
  :url "github.com/LSaldyt/ari"
  :license {:name "Eclipse Public License"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.2.4"]]
  :plugins [[lein-bin "0.3.4"]
            [lein-marginalia "0.9.1"]]
  :bin { :name "ari" }
  :main ari.core)
