(defproject runs "0.1.0"
  :description "Skeleton for a command line app"
  :url "github.com/LSaldyt"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.4"]]
  :plugins [[lein-bin "0.3.4"]]
  :bin { :name "app" }
  :main app.core)
