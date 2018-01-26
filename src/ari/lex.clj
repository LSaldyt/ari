(ns ari.lex)

(defn front-match? [a b]
  (= (first a) (first b)))

(defn match-separator [content separator c]
  (cond 
    (empty? separator) [true c]
    (empty? content)   false
    (front-match? content separator) (match-separator (rest content) (rest separator) (inc c))
    :else false))

(defn find-match [content separators]
  (let [result (some #(match-separator content % 0) separators)]
    (if (nil? result)
      [false 1]
      result)))

(defn separate-normal [content separators]
  (loop [separators separators
         remaining  content
         tokens     '()
         accum      ""]
    (let [[match n] (find-match remaining separators)]
      (if (empty? remaining)
        (remove #(= "" %) (concat tokens accum))
        (if match
          (recur separators
                 (subs remaining n)
                 (concat tokens (list accum (subs remaining 0 n)))
                 "")
          (recur separators
                 (subs remaining 1)
                 tokens
                 (str accum (first remaining))))))))

(def special-separators [["\"" "\"" :string]])

; (defn foo [m f]
;   (into {} (for [[k v] m] [k (f v)])))


(defn separate-special [content specials]
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
                   (subs remaining 0 n)
                   (get special-endings (subs remaining 0 n)))
            (recur (apply str (rest remaining))
                   tokens
                   (str accum (first remaining))
                   sep)))
        (let [result (match-separator remaining sep 0)]
          (if result 
            (let [[match n] result] 
              (recur (subs remaining n)
                     (concat tokens (list 
                                      [:special 
                                       (str accum (subs remaining 0 n))
                                       (get special-tags (subs remaining 0 n))]))
                     ""
                     nil))
            (recur (apply str (rest remaining))
                   tokens
                   (str accum (first remaining))
                   sep)))))))

(defn separate [content separators]
  (let [pre-separate (separate-special content special-separators)]
    (println pre-separate)
    (reduce concat
      (for [[pre-tag sub-content tag] pre-separate]
        (if (= pre-tag :special)
          (list [sub-content tag])
          (for [token (separate-normal sub-content separators)] [token :none]))))))

(defn create-taggers [tag-pairs]
  (for [[re tag] tag-pairs] 
    (list 
      (fn [input] 
        (re-matches re 
                    (str input))) tag)))

(defn do-tag [tokens taggers]
  (for [[token tag] tokens] 
    (if (= tag :none)
      (let [result 
            (some (fn [[p tag]] (if (p token) [token tag] false)) taggers)]
        (if (nil? result)
          [token "unknown"]
          result))
      [token tag])))

(defn lex [separators tag-pairs content]
  (println separators)
  (println (separate-special "this\"test\"this" special-separators))
  (println (separate "whoa,this\"test\"this" separators))
  (let [result
        (-> content
            (separate separators)
            (do-tag (create-taggers tag-pairs)))]
    (clojure.pprint/pprint result)
    result))
