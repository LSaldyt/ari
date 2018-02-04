(ns ari.parse)

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

; Basic parser elements:
; 
; x inOrder  
; x just
; x many
; x anyof

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
           (rest tokens)])
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
  (fn [tokens log] (loop [parsers   given-parsers
                      remaining tokens
                      tree  '()]
                 (if (empty? parsers)
                   [{:sequence tree} remaining]
                   (let [result ((first parsers) remaining log)]
                     (if (not result)
                       (do 
                         nil)
                       (let [[in-tree in-remaining] result]
                         (recur (rest parsers)
                                in-remaining
                                (if (nil? in-tree)
                                  tree
                                  (concat tree (list in-tree)))))))))))

(defn many [given-parser]
  (fn [tokens log] (loop [remaining tokens
                      values  '()]
                 (if (empty? remaining)
                   [{:values values} remaining]
                   (let [result (given-parser remaining log)]
                     (if (not result)
                       [{:values values} remaining]
                       (let [[in-values in-remaining] result]
                         (recur in-remaining
                                (concat values (list in-values))))))))))

(defn many1 [given-parser]
  (fn [tokens log]
    (let [[tree remaining] ((many given-parser) tokens log)]
      (if (empty? (:values tree))
        nil
        [tree remaining]))))

(defn from [parsers]
  (fn [tokens log] (loop [remaining-parsers parsers]
                 (if (empty? remaining-parsers)
                   nil
                   (let [result ((first remaining-parsers) tokens log)]
                     (if (not (nil? result))
                       result
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
        [{} tokens]))))

(defn discard [parser]
  (fn [tokens log]
    (let [result (parser tokens log)]
      (if result
        (let [[tree remaining] result]
          [nil remaining])
        nil))))

(defn- unsequence [[tree remaining]] 
  [(apply merge (:sequence tree)) remaining])

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
            [{} tokens])
          (let [[tree remaining] result]
            (let [in-result 
                  (((if one many1 many) (conseq [sep-parser item-parser])) 
                   remaining 
                   log)]
              (if (nil? in-result)
                (if one
                  nil
                  [tree remaining])
                (let [[in-tree in-remaining] in-result]
                  [{:values (concat (list tree) 
                                    (extract-sequences in-tree))} 
                   in-remaining])))))))))

(def sep-by  (create-sep-by false))
(def sep-by1 (create-sep-by true))

(defn create-ref-parser [dict k]
  (fn [tokens log] ((k dict) tokens)))

(defmacro create-parser [ident parser]
  `(fn [tokens# log#]
     (let [[tree# remaining#] (~parser tokens# log#)]
       (if (nil? tree#)
         nil
         [{(keyword '~ident) tree#} remaining#]))))

(defmacro defparser [ident parser]
  `(defn ~ident [tokens# log#]
     (let [[tree# remaining#] (~parser tokens# log#)]
       (if (nil? tree#)
         nil
         [{(keyword '~ident) tree#} remaining#]))))

(defn parse [parser content]
  (parser content {}))

