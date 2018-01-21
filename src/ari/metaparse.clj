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

(def separators [":" " " "(" ")" "{" "}" "'"])

(def direct-token (inorder [(token "'") (tag "name" :token) (token "'")]))
(def syntax-element (inorder [(tag "name" :name) (token ":") direct-token (tag "newline")]))

; (def test-tag-pairs [[#"^[0-9]*$" "int"]])
; [_a-zA-Z][_a-zA-Z0-9]{0,30}
; (defn read-source [infile parser separators tag-pairs]

(def tag-pairs [[#"[_a-zA-Z][_a-zA-Z0-9]{0,30}" "name"]
                [#"'" "quote"]
                [#"\n" "newline"]
                ]) 

(defn metaparse [filename]
  (let [tree 
        (read-source filename 
                     syntax-element 
                     separators 
                     tag-pairs)]
  tree))
