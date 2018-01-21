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
; [tokens] -> {tree}, [remaining tokens]

; assignment([[x ident] [= op] [2 int]]) -> {:assignment {:name x :op = :value 2}}

; Basic parser elements:
; 
; x inOrder  
; x just
; x many
; x anyof

(def any "*any*")

(defn token-matcher 
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

(defn token-matcher-wrapper [parser]
  (fn [tokens] 
    (let [result (parser (first tokens))]
      (if result
        (let [[token tag k] result]
          [(if (nil? k)
             {}
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


; [tokens] -> {tree}, [remaining tokens]

(defn inorder [given-parsers]
  (fn [tokens] (loop [parsers   given-parsers
                      remaining tokens
                      tree  {}]
                 (if (or (empty? remaining) (empty? parsers))
                   (if (empty? tree) nil [tree remaining])
                   (let [result ((first parsers) remaining)]
                     (if (not result)
                       (if (empty? tree) nil [tree remaining])
                       (let [[in-tree in-remaining] result]
                         (recur (rest parsers)
                                in-remaining
                                (merge tree in-tree)))))))))

(defn many [given-parser]
  (fn [tokens] (loop [remaining tokens
                      tree  '()]
                 (if (empty? remaining)
                   (if (empty? tree) nil [{:values tree} remaining])
                   (let [result (given-parser remaining)]
                     (if (not result)
                       (if (empty? tree) nil [{:values tree} remaining])
                       (let [[in-tree in-remaining] result]
                         (recur in-remaining
                                (concat (list in-tree) tree)))))))))

(defn any-of [parsers]
  (fn [tokens] (loop [remaining-parsers parsers]
                 (if (empty? remaining-parsers)
                   nil
                   (let [result ((first remaining-parsers) tokens)]
                     (if result
                       result
                       (recur (rest remaining-parsers))))))))


(defn parse [parser content]
  (parser content))

