(ns ari.metaparse
  (:require [ari.lex :refer [lex]]
            [ari.parse :refer :all]
            [ari.translate :refer [read-source]]))

; Simple pyBNF parser-generator form for testing:
; name: (form)
; where form can be a combination of:
; x 'if' -> token('if')
; x 'if keyword' -> (just 'if' 'keyword')
; '*any* int' -> (just any 'int')
; x (x | y | ..) -> (anyof [x y ..])
; x *(form) -> (many (form))
; `@key (form)` -> save form w/ key to AST

; (def test-separators ["=" " " ">>>" "\n"])
; (def test-tag-pairs [[#"^[0-9]*$" "int"]])
; (def test-parser (psequence-merge [(wild :name) (token " ") (token "=" :op) (token " ") (tag "int" :value) (wild)]))

(def separators [":" " " "(" ")" "{" "}" "'" "|" "\n" "." "!" "`" "@" "," "="])

(declare parser-part)

(def whitespace (discard (many (any-of [(token " ") (token "\n")]))))

(defparser direct-token (psequence-merge
                          [(token "'") 
                           (tag "name" :token) 
                           (optional (tag "space"))
                           (optional (tag "name" :tag)) 
                           (token "'")]))

(defparser or-operator (psequence-merge 
                         [(token "(")
                          (sep-by1
                            parser-part
                            (token "|"))
                          (token ")")]))

(defparser many-operator (psequence-merge 
                           [(token "*")
                            (token "(")
                            parser-part
                            (token ")")]))

(defparser key-operator (psequence-merge
                          [(token "`")
                           (token "@")
                           (tag "name" :key)
                           whitespace
                           parser-part
                           (token "`")]))

(def parser-part (any-of [or-operator
                          many-operator
                          key-operator
                          direct-token]))

(defparser syntax-element (psequence-merge [(tag "name" :name) 
                              (token ":") 
                              parser-part 
                              (tag "newline")]))

(defparser separator-def (psequence-merge
                           [(token "__separators__")
                            (token "=")
                            (sep-by (token any :sep) (token ","))
                            (tag "newline")]))

(defparser tagger-def (psequence-merge
                        [(token "__taggers__")
                         (token "=")
                         (token "{")
                         (sep-by (psequence-merge 
                                  [(token any :regex) 
                                   (token ":")
                                   (token any :tag)])
                                 (token ","))
                         (token "}")
                         (tag "newline")]))

(defparser bnf-file (psequence-merge 
                      [separator-def
                       tagger-def
                       (many syntax-element)]))

(defn create-syntax-element [tree]
  (let [{ident :name :as all} (:syntax-element tree)
        inner (dissoc all :name)
        inner-ident (first (keys inner))
        inner-tree (inner-ident inner)]
    (cond (= inner-ident :or-operator)
          (anyof (map create-syntax-element (:values inner-tree)))
          (= inner-ident :direct-token)
          ;(token ))


    inner-ident))

; (:or-operator
;  :direct-token
;  :many-operator
;  :key-operator)}


(defn process-bnf-file [tree]
  {:taggers (map #(list (first (:regex %)) (first (:tag %))) 
                 (:values (:tagger-def (:bnf-file tree))))
   :separators (map #(first (:sep %)) 
                    (:values (:separator-def (:bnf-file tree))))
   :parsers (map create-syntax-element (:values (:bnf-file tree)))
   })

(def tag-pairs [[#"[_a-zA-Z][_a-zA-Z0-9]{0,30}" "name"]
                [#"'" "quote"]
                [#"\n" "newline"]
                [#" " "space"]
                [#":" "colon"]
                [#"\|" "operator"] ]) 

(defn metaparse [filename]
  (let [[tree remaining] 
        (read-source filename 
                     bnf-file 
                     separators 
                     tag-pairs)]
  (clojure.pprint/pprint (process-bnf-file tree))
  tree))
