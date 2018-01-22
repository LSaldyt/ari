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
; (def test-parser (psequence [(wild :name) (token " ") (token "=" :op) (token " ") (tag "int" :value) (wild)]))

(def separators [":" " " "(" ")" "{" "}" "'" "|" "\n" "." "!" "`" "@" "," "="])

(declare parser-part)

(def whitespace (discard (many (any-of [(token " ") (token "\n")]))))

(defparser direct-token (psequence 
                          [(token "'") 
                           (tag "name" :token) 
                           (optional (tag "space"))
                           (optional (tag "name" :tag)) 
                           (token "'")]))

(defparser or-operator (psequence 
                         [(token "(")
                          (sep-by1
                            parser-part
                            (token "|"))
                          (token ")")]))

(defparser many-operator (psequence 
                           [(token "*")
                            (token "(")
                            parser-part
                            (token ")")]))

(defparser key-operator (psequence
                          [(token "`")
                           (token "@")
                           (tag "name" :key)
                           whitespace
                           parser-part
                           (token "`")]))

(defparser parser-part (any-of [or-operator
                                many-operator
                                key-operator
                                direct-token]))

(defparser syntax-element (psequence [(tag "name" :name) 
                              (token ":") 
                              parser-part 
                              (tag "newline")]))

(defparser separator-def (psequence
                           [(token "__separators__")
                            (token "=")
                            (sep-by (token any :sep) (token ","))
                            (tag "newline")]))

(defparser tagger-def (psequence
                        [(token "__taggers__")
                         (token "=")
                         (token "{")
                         (sep-by (psequence 
                                  [(token any :regex) 
                                   (token ":")
                                   (token any :tag)])
                                 (token ","))
                         (token "}")
                         (tag "newline")]))

(defparser bnf-file (psequence 
                      [separator-def
                       tagger-def
                       (many syntax-element)]))

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
