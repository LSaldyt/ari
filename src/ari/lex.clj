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

(defn separate [content separators]
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

(defn separate-special [content]
  (loop [remaining content
         tokens    '()
         accum     ""
         sep       nil] ; When sep is not nil, the function is in a string or comment
    (if (empty? remaining)
      (concat tokens (list accum))
      (if (nil? sep)
        (let [[match n] (find-match remaining ["\""])]
          (if match
            (recur (subs remaining n)
                   (concat tokens (list accum))
                   (subs remaining 0 n)
                   (subs remaining 0 n))
            (recur (apply str (rest remaining))
                   tokens
                   (str accum (first remaining))
                   sep)))
        (let [result (match-separator remaining sep 0)]
          (if result 
            (let [[match n] result] 
              (recur (subs remaining n)
                     (concat tokens (list (str accum (subs remaining 0 n))))
                     ""
                     nil))
            (recur (apply str (rest remaining))
                   tokens
                   (str accum (first remaining))
                   sep)))))))


(defn create-taggers [tag-pairs]
  (for [[re tag] tag-pairs] 
    (list 
      (fn [input] 
        (re-matches re 
                    (str input))) tag)))

(defn do-tag [tokens taggers]
  (for [token tokens] 
    (let [result 
          (some (fn [[p tag]] (if (p token) [token tag] false)) taggers)]
      (if (nil? result)
        [token "unknown"]
        result))))

(defn lex [separators tag-pairs content]
  (println (separate-special "this\"test\"this"))
  (let [result
        (-> content
            (separate separators)
            (do-tag (create-taggers tag-pairs)))]
    (clojure.pprint/pprint result)
    result))
