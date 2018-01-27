(ns ari.lex
  "General lexing module. Converts a raw text file into a list of tagged words based on defined lexical rules")

(defn front-match? 
  "Check if the leading elements of two sequences match"
  [a b]
  (= (first a) (first b)))

(defn match-separator 
  "Attempt to match a single separator to a string, counting the number of characters that match."
  [content separator c]
  (cond 
    (empty? separator) [true c]
    (empty? content)   false
    (front-match? content separator) (match-separator (rest content) (rest separator) (inc c))
    :else false))

(defn find-match 
  "Attempt to match a list of separators against a string."
  [content separators]
  (let [result (some #(match-separator content % 0) separators)]
    (if (nil? result)
      [false 1] ; For uniformity
      result)))

(defn separate-normal 
  "Break a string up by multi-character separators. Assumes that separators are in sorted order."
  [content separators]
  (loop [remaining  content
         tokens     '()
         accum      ""]
    (if (empty? remaining)
      ; Return a list of non-empty tokens
      (remove #(= "" %) (concat tokens (list accum))) 
      ; Check if the current remaining string starts with a separator
      (let [[match n] (find-match remaining separators)] 
        (if match
          ; If the remaining string starts with a separator, pull it out of the string and move onto the next part of the string
          (recur (subs remaining n)
                 (concat tokens (list accum (subs remaining 0 n)))
                 "")
          ; Otherwise, move on to the next character
          (recur (subs remaining 1)
                 tokens
                 (str accum (first remaining))))))))

(def special-separators [["\"" "\"" :string] ["'" "'" :string] ["#" "\n" :comment]])

(defn separate-special
  "Separate strings, comments, and other specially lexed elements from code"
  [content specials]
  (def special-endings (into {} (for [[k v t] specials] [k v])))
  (def special-tags (into {} (for [[k v t] specials] [k t])))
  (loop [remaining content
         tokens    '()
         accum     ""
         sep       nil] ; When sep is not nil, the function is in a string or comment
    (if (empty? remaining)
      (concat tokens (list [:normal accum :none]))
      (if (nil? sep)
        (let [[match n] (find-match remaining (map first specials))]
          (if match
            (recur (subs remaining n)
                   (concat tokens (list [:normal accum :none]))
                   "" ;(subs remaining 0 n)
                   (get special-endings (subs remaining 0 n)))
            (recur (subs remaining 1)
                   tokens
                   (str accum (first remaining))
                   sep)))
        (let [result (match-separator remaining sep 0)]
          (if result 
            (let [[match n] result] 
              (recur (subs remaining n)
                     (concat tokens (list 
                                      [:special 
                                       accum ;(str accum (subs remaining 0 n))
                                       (get special-tags (subs remaining 0 n))]))
                     ""
                     nil))
            (recur (subs remaining 1)
                   tokens
                   (str accum (first remaining))
                   sep)))))))

(defn separate
  "Preform lexing of both special and normal separators"
  [content separators]
  (let [pre-separate (separate-special content special-separators)]
    (reduce concat
      (for [[pre-tag sub-content tag] pre-separate]
        (if (= pre-tag :special)
          (list [sub-content tag])
          (for [token (separate-normal sub-content separators)] [token :none]))))))

(defn create-taggers 
  "Convert regular expressions into predicates"
  [tag-pairs]
  (for [[re tag] tag-pairs] 
    (list 
      (fn [input] 
        (re-matches re 
                    (str input))) tag)))

(defn do-tag 
  "Tag a list of tokens using regex-based predicates"
  [tokens taggers]
  (for [[token tag] tokens] 
    (if (= tag :none)
      (let [result 
            (some (fn [[p tag]] (if (p token) [token tag] false)) taggers)]
        (if (nil? result)
          [token "unknown"]
          result))
      [token tag])))

(defn lex 
  "Convert a string (file) into tagged tokens"
  [separators tag-pairs content]
  (let [result
        (-> content
            (separate separators)
            (do-tag (create-taggers tag-pairs)))]
    ;(clojure.pprint/pprint result)
    result))
