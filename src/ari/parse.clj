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
;   many
;   optional
;   inverse
;   anyof
;   allof

(def any "*any*")

(defn just 
  "((just any 'int') [2 'int']) -> [token tag]"
  ([etoken etag]
   (just etoken etag nil))
  ([etoken etag k]
    (fn [[token tag]]
      (if (and 
            (or (= etoken token) (= etoken any))
            (or (= etag tag)     (= etag any)))
        [token tag k]
        nil))))

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


(defn inorder [given-parsers]
  (fn [tokens] (loop [parsers   given-parsers
                      remaining tokens
                      consumed  {}]
                 (if (or (empty? remaining) (empty? parsers))
                   (if (empty? consumed) nil [consumed remaining])
                   (let [result ((first parsers) (first remaining))]
                     (if result 
                       (recur (rest parsers)
                              (rest remaining)
                              (let [[token tag k] result] 
                                (if (nil? k)
                                  consumed
                                  (assoc consumed k [token tag]))))
                       (if (empty? consumed) nil [consumed remaining])))))))

; (defn many [given-parser]
;   (fn [tokens] (loop [remaining tokens
;                       consumed  {}]
;                  (if (empty? remaining)
;                    (if (empty? consumed) nil [consumed remaining])
;                    (let [result (given-parser tokens)]
;                      (if result
;                        (recur )
;                        (if (empty? consumed) nil [consumed remaining])
; 
;                        )
;                      )
;                    )
; 
;                  )))

(def test-assignment (inorder [(wild :name) (token " ") (token "=" :op) (token " ") (tag "int" :value) (wild)]))

(defn parse [content]
  (println content)
  (println (test-assignment content))
  content)
