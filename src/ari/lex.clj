(ns ari.lex)

(def test-separators ["=" " " ">>>" "\n"])

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

(def test-tag-pairs [[#"^[0-9]*$" :int]])
(def test-tag-functions (for [[re tag] test-tag-pairs] (list (fn [input] (re-matches re (str input))) tag)))

(defn tag [tokens tag-vec]
  (for [token tokens] 
    (let [result 
          (some (fn [[p tag]] (if (p token) [token tag] false)) tag-vec)]
      (if (nil? result)
        [token :unknown]
        result))))

(defn lex [content]
  (-> content
      (separate test-separators)
      (tag      test-tag-functions)))
