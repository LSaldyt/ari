(ns ari.experimental.condenser)

(defn condense-lisp-inner [tree]
  )

(defn condense-lisp [tree]
  (clojure.pprint/pprint (:values (:body tree))))

