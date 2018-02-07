(ns ari.parse.parse
  "Ari's parsing module:

   The core of ari's input: this module allows for the EBNF/pyBNF grammars to be specified, so that other grammars can be read in by ari.
   Other grammars are then converted to this set of parser-functions.

   Parser structure
   [tokens log] -> [{tree}, [remaining tokens log]]
   If the parser is unsuccessful, then {tree} will actually be nil, signifying error.
   The log argument is a dictionary, usually with the keys :all and :head defined as a subdictionary and key-list respectively.
   
   Notes:

   Parse technique:
   Given multiple parsers:
   While tokens are available for input..
   For each available parser, try its first element
   If this matches, keep the parser in the group of possible parsers
   If it does not match, remove the parser from the current possible set
   If there are no parsers left, throw an error.

   TODO: Once there is no input left, create a parse tree with the longest parser. If two syntax elements are ambiguous, throw an error.

  "

  (:require [ari.log :as log]
            [ari.parse.base :refer :all]))


(defn conseq [given-parsers]
  (fn [tokens log] 
    (loop [parsers   given-parsers
           remaining tokens
           tree      '()
           loop-log  (log/log log "Began conseq")]
      (if (empty? parsers)
        [{:sequence tree} remaining (log/log loop-log (str "Conseq Success"))]
      (let [[in-tree in-remaining in-log] 
            (use-parser (first parsers) remaining loop-log)]
        (if (nil? in-tree)
          [nil in-remaining (log/log in-log "Stopped conseq")]
          (recur (rest parsers)
                 in-remaining
                 ; Trust sub-parsers to only provide valid maps. 
                 ; Don't bother removing empty ones (They'll be summed with conseq-merge anyway).
                 ; Err... (address this)
                 (concat tree (list in-tree));(remove empty? (list in-tree)))
                 in-log)))))))

(defn many [given-parser]
  (fn [tokens log] 
    (loop [remaining tokens
           values  '()
           loop-log (log/log log "Began Many")]
      (if (empty? remaining)
        [{:values values} 
         remaining 
         (log/log loop-log (str "Many success (no more tokens)"))]
        (let [[in-values in-remaining in-log] 
              (use-parser given-parser remaining loop-log)]
          (if (nil? in-values)
            [{:values values} remaining (log/log in-log "Many Success")]
            (recur in-remaining
                   (concat values (list in-values))
                   in-log)))))))

(defn many1 [given-parser]
  (fn [tokens log]
    (let [log (log/log log "Began many1")
          [tree remaining in-log] (use-parser (many given-parser) tokens log)]
      (if (empty? (:values tree))
        [nil remaining (log/log in-log "Many1 failure")]
        [tree remaining (log/log in-log (str "Many1 Success"))]))))

(defn from [parsers]
  (fn [tokens log] 
    (let [log (log/log log "Began from")]
      (loop [remaining-parsers parsers]
        (if (empty? remaining-parsers)
          [nil tokens (log/log log "From failure")]
          (let [[tree remaining in-log] 
                (use-parser (first remaining-parsers) tokens log)]
            (if (not (nil? tree))
              [tree 
               remaining 
               (log/log in-log "From Success")]
              (recur (rest remaining-parsers)))))))))

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
           (some #(= (fn-name x) (fn-name %)) out-parsers)) 
         parsers)))

(defn optional [parser]
  (fn [tokens log] 
    (let [log (log/log log "Begin Optional")
          [tree remaining in-log] (use-parser parser tokens log)]
      [(if (nil? tree) {} tree)
       remaining
       (log/log in-log (str "Ran optional (" 
                            (if (nil? tree) "unsucessfully" "Successfully") ")"
                            ))])))

(defn discard [parser]
  (fn [tokens log]
    (let [log (log/log log "Began Discard")
          [tree remaining in-log] (use-parser parser tokens log)]
      (if (nil? tree)
        [nil remaining (log/log in-log "Discard failure")]
        [{} remaining (log/log in-log "Discard Success")]))))

(defn- unsequence [[tree remaining log]] 
  [(apply merge (:sequence tree)) remaining log])

(defn conseq-merge [given-parsers]
  (fn [tokens log]
    (unsequence ((conseq given-parsers) tokens log))))

(defn- extract-sequences [tree]
  (map #(first (map merge (remove empty? (:sequence %)))) (:values tree)))

(defn- create-sep-by [one]
  (fn [item-parser sep-parser]
    (fn [tokens log]
      (let [[tree remaining log1] (use-parser item-parser tokens log)]
        (if (nil? tree)
          (if one
            [nil remaining log1]
            [{} remaining log1])
          (let [[in-tree in-remaining in-log]
                (((if one many1 many) (conseq [sep-parser item-parser])) 
                 remaining 
                 log1)]
            (if (nil? in-tree)
              (if one
                [nil in-remaining in-log]
                [tree in-remaining in-log])
              [{:values (concat (list tree) 
                                (extract-sequences in-tree))} 
               in-remaining
               in-log])))))))

(def sep-by  (create-sep-by false))
(def sep-by1 (create-sep-by true))

(defmacro create-parser [ident parser]
  `(fn [tokens# log#]
     (let [[tree# remaining# inlog#] (use-parser ~parser tokens# log#)]
       (if (nil? tree#)
         [nil remaining# inlog#]
         [{(keyword '~ident) tree#} remaining# inlog#]))))

(defmacro defparser [ident parser]
  `(defn ~ident [tokens# log#]
     (let [[tree# remaining# inlog#] (use-parser ~parser tokens# log#)]
       (if (nil? tree#)
         [nil remaining# inlog#]
         [{(keyword '~ident) tree#} remaining# inlog#]))))

(defn retrieve [k ptree-atom]
  (fn [tokens log] 
    (let [log (log/log log "Begin retrieve")
          result ((get @ptree-atom k) tokens log)]
      result)))

(defn parse [parser content]
  (println "Parsing " );parser)
  (parser content {:head [:all]}))

