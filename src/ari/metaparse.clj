(ns ari.metaparse
  (:require [ari.lex :refer [lex]]
            [ari.parse :refer :all]
            [ari.translate :refer [read-source]]))

; Simple pyBNF parser-generator form for testing:
; name: (form)
; where form can be a combination of:
; 'if' -> token('if')
; 'if keyword' -> (just 'if' 'keyword')
; '*any* int' -> (just any 'int')
; x | y | .. -> (anyof [x y ..])
; *(form) -> (many (form))
; `@key (form)` -> save form w/ key to AST

; (def test-separators ["=" " " ">>>" "\n"])
; (def test-tag-pairs [[#"^[0-9]*$" "int"]])
; (def test-parser (inorder [(wild :name) (token " ") (token "=" :op) (token " ") (tag "int" :value) (wild)]))

(def separators [":" " " "(" ")" "{" "}" "'" "|" "\n" "." "!"])

(defparser direct-token (inorder [
                            (token "'") 
                            (tag "name" :token) 
                            (optional (tag "space"))
                            (optional (tag "name" :tag)) 
                            (token "'")]))
(defparser or-operator (sep-by1
                        direct-token 
                        (token "|")))

(defparser parser-part (any-of [or-operator
                                direct-token]))

(defparser syntax-element (inorder [(tag "name" :name) 
                              (token ":") 
                              parser-part 
                              (tag "newline")]))

(defparser bnf-file (many syntax-element))

(def tag-pairs [[#"[_a-zA-Z][_a-zA-Z0-9]{0,30}" "name"]
                [#"'" "quote"]
                [#"\n" "newline"]
                [#" " "space"]
                [#":" "colon"]
                [#"\|" "operator"]
                ]) 

(defn metaparse [filename]
  (let [tree 
        (read-source filename 
                     bnf-file 
                     separators 
                     tag-pairs)]
  tree))
