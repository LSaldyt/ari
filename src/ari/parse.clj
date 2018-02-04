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

(defn- token-matcher-wrapper [parser]
  (fn [tokens log] 
    (let [result (parser (first tokens))]
      (if result
        (let [[token tag k] result]
          [(if (nil? k)
             nil
             {k [token tag]})
           (rest tokens)
           (log/log log (str "Success: " parser))])
        nil))))

(defn just 
  ([etoken etag]
   (just etoken etag nil))
  ([etoken etag k]
   (token-matcher-wrapper (token-matcher etoken etag k))))

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
                   (let [result ((first parsers) remaining loop-log)]
                     (if (not result)
                       (do 
                         nil)
                       (let [[in-tree in-remaining in-log] result ]
                         (recur (rest parsers)
                                in-remaining
                                (if (nil? in-tree)
                                  tree
                                  (concat tree (list in-tree)))
                                (log/join log in-log)))))))))

(defn many [given-parser]
  (fn [tokens log] (loop [remaining tokens
                      values  '()]
                 (if (empty? remaining)
                   [{:values values} remaining (log/log log (str "Many success"))]
                   (let [result (given-parser remaining log)]
                     (if (not result)
                       [{:values values} remaining log]
                       (let [[in-values in-remaining in-log] result]
                      ; TODO: Join logs
                         (recur in-remaining
                                (concat values (list in-values))))))))))

(defn many1 [given-parser]
  (fn [tokens log]
    (let [[tree remaining in-log] ((many given-parser) tokens log)]
      ; TODO: Join logs
      (if (empty? (:values tree))
        nil
        [tree remaining (log/log log (str "Many1 success"))]))))

(defn from [parsers]
  (fn [tokens log] (loop [remaining-parsers parsers]
                 (if (empty? remaining-parsers)
                   nil
                   (let [result ((first remaining-parsers) tokens log)]
                     (if (not (nil? result))
                       (let [[tree remaining in-log] result]
                         ; TODO: merge logs
                         [tree remaining (log/log in-log "From success")])
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
    (let [result (parser tokens log)]
      (if result
        result
        [{} tokens (log/log log (str "Optional skipped"))]))))

(defn discard [parser]
  (fn [tokens log]
    (let [result (parser tokens log)]
      (if result
        (let [[tree remaining] result]
          [nil remaining (log/log log (str "Discard success"))])
        nil))))

(defn- unsequence [[tree remaining log]] 
  [(apply merge (:sequence tree)) remaining log])

(defn conseq-merge [given-parsers]
  (fn [tokens log]
    (unsequence ((conseq given-parsers) tokens log))))

(defn- extract-sequences [tree]
  (map #(first (:sequence %)) (:values tree)))

(defn- create-sep-by [one]
  (fn [item-parser sep-parser]
    (fn [tokens log]
      (let [result (item-parser tokens log)]
        (if (nil? result)
          (if one
            nil
            [{} tokens log])
          (let [[tree remaining log1] result]
            ; TODO: join logs
            (let [in-result 
                  (((if one many1 many) (conseq [sep-parser item-parser])) 
                   remaining 
                   log)]
              (if (nil? in-result)
                (if one
                  nil
                  [tree remaining log])
                (let [[in-tree in-remaining in-log] in-result]
                  ; TODO: join logs
                  [{:values (concat (list tree) 
                                    (extract-sequences in-tree))} 
                   in-remaining
                   in-log])))))))))

(def sep-by  (create-sep-by false))
(def sep-by1 (create-sep-by true))

(defn create-ref-parser [dict k]
  (fn [tokens log] ((k dict) tokens log)))

(defmacro create-parser [ident parser]
  `(fn [tokens# log#]
     (let [[tree# remaining#] (~parser tokens# log#)]
       (if (nil? tree#)
         nil
         [{(keyword '~ident) tree#} remaining# log#]))))

(defmacro defparser [ident parser]
  `(defn ~ident [tokens# log#]
     (let [[tree# remaining#] (~parser tokens# log#)]
       (if (nil? tree#)
         nil
         [{(keyword '~ident) tree#} remaining# log#]))))

(defn retrieve [k ptree-atom]
  (fn [tokens log] ((get @ptree-atom k) tokens (log/log log (str "Retrieved " k)))))

(defn parse [parser content]
  (parser content {:head [:all]}))

