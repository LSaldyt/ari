(ns ari.metaparse
  (:require [ari.parse :refer [many any-of tag token wild inorder]]
            [ari.lex :refer [lex]]))

; Simple pyBNF parser-generator form for testing:
; name: (form)
; where form can be a combination of:
; 'if' -> token('if')
; 'if keyword' -> (just 'if' 'keyword')
; '*any* int' -> (just any 'int')
; x | y | .. -> (anyof [x y ..])
; *(form) -> (many (form))
; `@key (form)` -> save form w/ key to AST

(def form (token "test"))
(def syntax-element (inorder [(tag "name" :name) (token ":") form]))

(defn metaparse []
  '())
