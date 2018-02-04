(ns ari.parse
  (:require [ari.log :as log]))

; Parse technique:
; Given multiple parsers:
; While tokens are available for input..
; For each available parser, try its first element
; If this matches, keep the parser in the group of possible parsers
; If it does not match, remove the parser from the current possible set
; If there are no parsers left, throw an error.
; Once there is no input left, create a parse tree with the longest parser. If two syntax elements are ambiguous, throw an error.

; Parser structure
; [tokens log] -> {tree}, [remaining tokens]

; assignment([[x ident] [= op] [2 int]]) -> {:assignment {:name x :op = :value 2}}

(defn use-parser [parser tokens log]
  (let [log (log/log-push log "test")
        [tree remaining in-log] (parser tokens log)
        in-log (log/join log in-log)
        in-log (log/log-pop in-log)]
    [tree remaining in-log]))

(def any "*any*")

(defn- token-matcher 
  "((just any 'int') [2 'int']) -> [token tag]"
  ([etoken etag]
   (token-matcher etoken etag nil))
  ([etoken etag k]
    (fn [[token tag]]
      (if (and 
            (or (= etoken token) (= etoken any))
            (or (= etag tag)     (= etag any)))
        [token tag k]
        nil))))

(defn- token-matcher-wrapper [parser etoken etag]
  (fn [tokens log] 
    (let [result (parser (first tokens))]
      (if result
        (let [[token tag k] result]
          [(if (nil? k)
             {}
             {k [token tag]})
           (rest tokens)
           (log/log log (str "Success: <" etoken ">, <" etag ">"))])
        [nil tokens log]))))

(defn just 
  ([etoken etag]
   (just etoken etag nil))
  ([etoken etag k]
   (token-matcher-wrapper (token-matcher etoken etag k) etoken etag)))

(defn tag
  ([etag]
    (just any etag))
  ([etag k]
    (just any etag k)))

(defn token
  ([etoken]
    (just etoken any))
  ([etoken k]
    (just etoken any k)))

(defn wild
  ([] (just any any))
  ([k] (just any any k)))

(defn conseq [given-parsers]
  (fn [tokens log] (loop 
                     [parsers   given-parsers
                      remaining tokens
                      tree  '()
                      loop-log log]
                 (if (empty? parsers)
                   [{:sequence tree} remaining (log/log loop-log (str "Conseq success"))]
                 (let [[in-tree in-remaining in-log] 
                       (use-parser (first parsers) remaining loop-log)]
                   (if (nil? in-tree)
                     [nil tokens in-log]
                     (recur (rest parsers)
                            in-remaining
                            (if (nil? in-tree)
                              tree
                              (concat tree (list in-tree)))
                            in-log)))))))

(defn many [given-parser]
  (fn [tokens log] (loop 
                     [remaining tokens
                      values  '()
                      loop-log log]
                 (if (empty? remaining)
                   [{:values values} 
                    remaining 
                    (log/log loop-log (str "Many success"))]
                   (let [[in-values in-remaining in-log] 
                         (use-parser given-parser remaining loop-log)]
                     (if (nil? in-values)
                       [{:values values} tokens in-log]
                     (recur in-remaining
                            (concat values (list in-values))
                            in-log)))))))

(defn many1 [given-parser]
  (fn [tokens log]
    (let [[tree remaining in-log] (use-parser (many given-parser) tokens log)]
      (if (empty? (:values tree))
        [nil tokens in-log]
        [tree remaining (log/log in-log (str "Many1 success"))]))))

(defn from [parsers]
  (fn [tokens log] (loop [remaining-parsers parsers]
                 (if (empty? remaining-parsers)
                   [nil tokens log]
                   (let [[tree remaining in-log] 
                         (use-parser (first remaining-parsers) tokens log)]
                     (if (not (nil? tree))
                       [tree 
                        remaining 
                        (log/log in-log "From success")]
                       (recur (rest remaining-parsers))))))))

(defn- demunge-fn
  [fn-object]
  (let [dem-fn (str fn-object)
        pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem-fn))]
    (if pretty pretty dem-fn)))

(defn- fn-name [fn-object]
  (let [s (demunge-fn fn-object)]
    (subs s 0 (- (count s) 9))))

(defn any-except [parsers & out-parsers]
  (from (remove 
            (fn [x] 
              (some #(= (fn-name x) (fn-name %)) out-parsers)) parsers)))

(defn optional [parser]
  (fn [tokens log] 
    (let [[tree remaining in-log] (use-parser parser tokens log)]
      [(if (nil? tree) {} tree)
       remaining
       in-log])))

(defn discard [parser]
  (fn [tokens log]
    (let [[tree remaining in-log] (use-parser parser tokens log)]
      [nil 
       (if (nil? tree) tokens remaining)
       (log/log in-log (str "Discard success"))])))

(defn- unsequence [[tree remaining log]] 
  ;(clojure.pprint/pprint (:sequence tree))
  [(apply merge (:sequence tree)) remaining log])

(defn conseq-merge [given-parsers]
  (fn [tokens log]
    (unsequence ((conseq given-parsers) tokens log))))

(defn- extract-sequences [tree]
  (map #(first (:sequence %)) (:values tree)))

(defn- create-sep-by [one]
  (fn [item-parser sep-parser]
    (fn [tokens log]
      (let [[tree remaining log1] (use-parser item-parser tokens log)]
        (if (nil? tree)
          (if one
            [nil tokens log1]
            [{} tokens log1])
          (let [[in-tree in-remaining in-log]
                (((if one many1 many) (conseq [sep-parser item-parser])) 
                 remaining 
                 log1)]
            (if (nil? in-tree)
              (if one
                [nil tokens in-log]
                [tree remaining in-log])
              [{:values (concat (list tree) 
                                (extract-sequences in-tree))} 
               in-remaining
               in-log])))))))

(def sep-by  (create-sep-by false))
(def sep-by1 (create-sep-by true))

(defn create-ref-parser [dict k]
  (fn [tokens log] ((k dict) tokens log)))

(defmacro create-parser [ident parser]
  `(fn [tokens# log#]
     (let [[tree# remaining# inlog#] (use-parser ~parser tokens# log#)]
       (if (nil? tree#)
         [nil tokens# inlog#]
         [{(keyword '~ident) tree#} remaining# inlog#]))))

(defmacro defparser [ident parser]
  `(defn ~ident [tokens# log#]
     (let [[tree# remaining# inlog#] (use-parser ~parser tokens# log#)]
       (if (nil? tree#)
         [nil tokens# inlog#]
         [{(keyword '~ident) tree#} remaining# inlog#]))))

(defn retrieve [k ptree-atom]
  (println "Invoked retrieve")
  (clojure.pprint/pprint @ptree-atom)
  (fn [tokens log] 
    (let [result ((get @ptree-atom k) tokens log )]
      (println result)
      result)))

(defn parse [parser content]
  (println parser)
  (parser content {:head [:all]}))

