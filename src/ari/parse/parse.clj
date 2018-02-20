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
  "Parse a list of given parsers, one after another.
   If one parser fails, this entire parser fails."
  (fn [tokens log] 
    (loop [parsers   given-parsers
           remaining tokens
           tree      '()
           loop-log  (log/log log "Began conseq")]
      (if (empty? parsers)
        [{:sequence tree} remaining (log/commit loop-log "Conseq Success" tree)]
      (let [[in-tree in-remaining in-log] 
            (use-parser (first parsers) remaining loop-log)]
        (if (nil? in-tree)
          [nil in-remaining (log/commit in-log "Stopped conseq" in-tree)]
          (recur (rest parsers)
                 in-remaining
                 ; Trust sub-parsers to only provide valid maps. 
                 ; Don't bother removing empty ones (They'll be summed with conseq-merge anyway).
                 ; Err... (address this)
                 (concat tree (list in-tree));(remove empty? (list in-tree)))
                 (log/commit in-log "Continuing conseq" in-tree))))))))

(defn- unsequence [[tree remaining log]] 
  "Helper function for conseq-merge"
  [(apply merge (:sequence tree)) remaining log])

(defn conseq-merge [given-parsers]
  "Same functionality as conseq (Parse given parsers in order, failing if any of them fail).
  The output tree generated by conseq-merge will turn the list of values into a tree, instead.
  This assumes that parser list provides dictionary list-elements with unique keys. (Which is usually the case)"
  (fn [tokens log]
    ;((conseq given-parsers) tokens log)))
    (unsequence ((conseq given-parsers) tokens log))))


(defn many [given-parser]
  "Parse a single parser as many times as possible, doesn't fail."
  (fn [tokens log] 
    (loop [remaining tokens
           values  '()
           loop-log (log/commit log "Began Many" log)]
      (if (empty? remaining)
        [{:values values} 
         remaining 
         (log/commit loop-log (str "Many success (no more tokens)") loop-log)]
        (let [[in-values in-remaining in-log] 
              (use-parser given-parser remaining loop-log)]
          (if (nil? in-values)
            [{:values values} remaining (log/commit in-log "Many Success" in-log)]
            (recur in-remaining
                   (concat values (list in-values))
                   (log/commit in-log "Continuing Many" in-log))))))))

(defn many1 [given-parser]
  "Parse a single parser at least once."
  (fn [tokens log]
    (let [log (log/commit log "Began many1" log)
          [tree remaining in-log] (use-parser (many given-parser) tokens log)]
      (if (empty? (:values tree))
        [nil remaining (log/commit in-log "Many1 failure" nil)]
        [tree remaining (log/commit in-log (str "Many1 Success") tree)]))))

(defn from [parsers]
  "Parse any parser from a list.
   Currently, if the first parser succeeds, stops parsing."
  ; TODO: continue parsing and choose best tree?
  ; Either way, this parser is one of the most important
  ;(println (count parsers))
  ;(println parsers)
  (fn [tokens log] 
    (let [log (log/commit log "Began from" log)]
      (loop [remaining-parsers parsers]
        (if (empty? remaining-parsers)
          [nil tokens (log/commit log "From failure" log)]
          (let [[tree remaining in-log] 
                (use-parser (first remaining-parsers) tokens log)]
            (if (not (nil? tree))
              [tree 
               remaining 
               (log/commit in-log "From Success" log)]
              (recur (rest remaining-parsers)))))))))

(defn from-except [parsers out-parsers]
  "Apply the from function to a list of parsers, excluding others"
  ;(println (doall (map #(fn-name %) out-parsers)))
  ;(println (doall (map #(fn-name %) parsers)))
  (let [in-parsers 
        (remove 
           (fn [x] 
             ;(println (fn-name x))
             ;(println (doall (map #(fn-name %) out-parsers)))
             (some #(= (fn-name x) (fn-name %)) out-parsers)) 
           parsers)]
  (partial use-parser
    (from in-parsers))))

(defn optional [parser]
  "Optionally parse a parser"
  (fn [tokens log] 
    (let [log (log/log log "Begin Optional")
          [tree remaining in-log] (use-parser parser tokens log)]
      [(if (nil? tree) {} tree)
       remaining
       (log/commit in-log (str "Ran optional (" 
                            (if (nil? tree) "unsucessfully" "Successfully") ")") 
                   tree)])))

(defn discard [parser]
  "Require a parser to pass, but intentionally discard its result.
  This is useful for whitespace parsing and similar"
  (fn [tokens log]
    (let [log (log/log log "Began Discard")
          [tree remaining in-log] (use-parser parser tokens log)]
      (if (nil? tree)
        [nil remaining (log/commit in-log "Discard failure" in-log)]
        [{} remaining (log/commit in-log "Discard Success" in-log)]))))

(defn- extract-sequences [tree]
  "Helper function for the sep-by functions"
  (map #(first (map merge (remove empty? (:sequence %)))) (:values tree)))

(defn- create-sep-by [one]
  "Helper function for the sep-by functions.
  This unites sepby-functionality between sepby1 and sepby"
  (fn [item-parser sep-parser]
    (fn [tokens log]
      (let [[tree remaining log1] (use-parser item-parser tokens log)]
        (if (nil? tree)
          (if one
            [nil remaining (log/commit log1 "Sepby failed" nil)]
            [{} remaining (log/commit log1 "Sepby stopped w/ no result" {})])
          (let [[in-tree in-remaining in-log]
                (((if one many1 many) (conseq [sep-parser item-parser])) 
                 remaining 
                 log1)]
            (if (nil? in-tree)
              (if one
                [nil in-remaining (log/commit in-log "Sepby failed after separator" nil)]
                [tree in-remaining (log/commit in-log "Sepby stopped after first element" tree)])
              [{:values (concat (list tree) 
                                (extract-sequences in-tree))} 
               in-remaining
               (log/commit in-log "Sepby finished successfully" in-tree)])))))))

"Separate one parser by another (i.e. a|b|c)"
(def sep-by  (create-sep-by false))
"Separate one parser by another (i.e. a|b|c), requiring at least one separation"
(def sep-by1 (create-sep-by true))

(defmacro create-parser [ident parser]
  "Create an unnamed parser that tags its resulting tree with the keyword ident"
  `(fn [tokens# log#]
     (let [log# (log/log log# (str "Began " ~ident)) 
           [tree# remaining# inlog#] (use-parser ~parser tokens# log#)]
       ;(println (type ~ident))
       ;(println ~ident)
       ;(println (keyword ~ident))
       ;(println (keyword '~ident))
       ;(println (keyword ~ident))
       ;(/ 1 0)
       (if (nil? tree#)
         [nil 
          remaining#
          (log/commit inlog# (str "Finished " ~ident " unsuccessfully") {:tree nil})]
         [{(keyword ~ident) tree#} 
          remaining# 
          (log/commit inlog# (str "Finished " ~ident " successfully") {:tree tree#})]))))

(defmacro defparser [ident parser]
  "Create an named parser (named $ident) that tags its resulting tree with the keyword $ident"
  `(def ~ident (create-parser '~ident ~parser)))

(defn retrieve [k ptree-atom]
  "Retrieve another parser by name from an atomic dictionary"
  (fn [tokens log] 
    (let [log (log/log log "Retrieve")
          result ((get @ptree-atom k) tokens log)]
      result)))

(defn parse [parser log content]
  (println "Parsing " content)
  ;(println log)
  [(parser content log) (log/log log "Parsing finished")])

(def whitespace (optional (discard (many (from [(tag "space") (tag "newline")])))))
(def space (optional (discard (many (tag "space")))))
(defn white [parser] (conseq-merge [whitespace parser whitespace]))
(defn spaced [parser] (conseq-merge [space parser space]))
(defn ordered [parsers] (conseq-merge (map white parsers)))
