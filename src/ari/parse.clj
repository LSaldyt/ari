(ns ari.parse)

; Parse technique:
; Given multiple parsers:
; While tokens are available for input..
; For each available parser, try its first element
; If this matches, keep the parser in the group of possible parsers
; If it does not match, remove the parser from the current possible set
; If there are no parsers left, throw an error.
; Once there is no input left, create a parse tree with the longest parser. If two syntax elements are ambiguous, throw an error.

; Basic parser elements:
; inOrder 
; just
; optional
; inverse
; anyof
; allof
; anycard
; many

(def any "*any*")

(defn just 
  ([etoken]
    (fn [[token tag]] 
      (or (= token etoken) (= etoken any))))
  ([etoken etag]
    (fn [[token tag]]
      (and 
        (or (= etoken token) (= etoken any))
        (or (= etag tag)     (= etag any))))))

(defn inorder [given-parsers]
  (fn [tokens] (loop [parsers   given-parsers
                      remaining tokens]
                 (if (or (empty? remaining) (empty? parsers))
                   true
                   (and ((first parsers) (first remaining))
                     (recur (rest parsers)
                             (rest remaining)))))))

(def test-assignment (inorder [(just any) (just " ") (just "=") (just " ") (just any "int") (just any)]))

;; (println (first [[" " "whitespace"]]))
;; (println ((inorder [(just any) (just " ")]) [["a" "test"] [" " "whitespace"]]))
;; (println (test-assignment [["a" "ident"] [" " "whitespace"] ["=" "operator"] [" " "whitespace"] ["2" "int"]]))
;; (println ((just any) ["a" "ident"]))
;; (println ((just " ") [" " "whitespace"]))
;; (println ((just "=") ["=" "operator"]))
;; (println ((just " ") [" " "whitespace"]))
;; (println ((just any "int") ["2" "int"]))

(defn parse [content]
  (println content)
  (println (test-assignment content))
  content)
