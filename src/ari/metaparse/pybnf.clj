(ns ari.metaparse.pybnf
  (:require [ari.lex :refer [lex]]
            [ari.parse.parse :refer :all]
            [ari.parse.base :refer :all]
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
; (def test-parser (conseq-merge [(wild :name) (token " ") (token "=" :op) (token " ") (tag "int" :value) (wild)]))

(def separators [":" " " "(" ")" "{" "}" "'" "|" "\n" "." "!" "`" "@" "," "="])

(declare parser-part)

(defparser identifier (tag "name" :identifier))
(defparser direct-token (tag :string :token))

(defparser or-operator (conseq-merge 
                         [(token "(")
                          (sep-by1
                            parser-part
                            (token "|"))
                          (token ")")]))

(defparser many-operator (conseq-merge 
                           [(token "*")
                            (token "(")
                            parser-part
                            (token ")")]))

(defparser key-operator (conseq-merge
                          [(token "`")
                           (token "@")
                           (tag "name" :key)
                           whitespace
                           parser-part
                           (token "`")]))

(def parser-part (from [or-operator
                        many-operator
                        key-operator
                        direct-token
                        identifier
                        ]))

(defparser syntax-element (conseq-merge [(tag "name" :name) 
                              (token ":") 
                              parser-part 
                              (tag "newline")]))

(defparser separator-def (conseq-merge
                           [(token "__separators__")
                            (token "=")
                            (sep-by (tag :string :sep) (token ","))
                            (tag "newline")]))

(defparser tagger-def (conseq-merge
                        [(token "__taggers__")
                         (token "=")
                         (token "{")
                         (sep-by (conseq-merge 
                                  [(tag :string :regex) 
                                   (token ":")
                                   (tag :string :tag)])
                                 (token ","))
                         (token "}")
                         (tag "newline")]))

(defparser bnf-file (conseq-merge 
                      [separator-def
                       tagger-def
                       (many syntax-element)]))

(defn replace-special [item]
  (cond (= item "NEWLINE")
        "\n"
        (= item "SPACE")
        " "
        (= item "TAB")
        "\t"
        :else
        item))

(defn create-direct-token [tree]
  (just (replace-special (first (get tree :token [any])))
        (replace-special (first (get tree :tag [any])))))

(defn create-syntax-element [tree]
  (let [inner-ident (first (keys tree))
        inner-tree (inner-ident tree)]
    (cond (= inner-ident :or-operator)
          (from (map create-syntax-element (:values inner-tree)))
          (= inner-ident :direct-token)
          (create-direct-token inner-tree)
          (= inner-ident :key-operator)
          (create-parser (symbol (first (:key inner-tree))) (create-syntax-element (dissoc inner-tree :key)))
          (= inner-ident :many-operator)
          (many (create-syntax-element inner-tree)))))

(defn outer-create-syntax-element [tree]
  (let [{ident :name :as all} (:syntax-element tree)
        inner (dissoc all :name)]
    (let [result 
          (create-syntax-element inner)]
      result)))


(defn process-bnf-file [tree]
  {:taggers (map #(list (first (:regex %)) (first (:tag %))) 
                 (:values (:tagger-def (:bnf-file tree))))
   :separators (map #(first (:sep %)) 
                    (:values (:separator-def (:bnf-file tree))))
   :parsers (map outer-create-syntax-element (:values (:bnf-file tree)))})

(defn add-to-bnf-file [tree]
  (let [separators (:separators tree)]
    (assoc tree :separators (concat separators (list "\n")))))

(def tag-pairs [[#"[_a-zA-Z][_a-zA-Z0-9]{0,30}" "name"]
                [#"'" "quote"]
                [#"\n" "newline"]
                [#" " "space"]
                [#"\\|" "operator"]
                [#":" "colon"]])

(def special-separators [["\"" "\"" :string] ["'" "'" :string] ["#" "\n" :comment]])

(defn create-metaparser [bnf-file-tree-clean]
  (fn [filename] (read-source filename
                              (many (from (:parsers bnf-file-tree-clean)))
                              (:separators bnf-file-tree-clean)
                              special-separators
                              (map 
                                #(list (re-pattern (first %)) (second %)) 
                                (:taggers bnf-file-tree-clean))
                              {:head [:all]}
                              )))

(defn pybnf [filename testfile]
  (let [[tree remaining log] 
        (read-source filename 
                     bnf-file 
                     separators 
                     special-separators
                     tag-pairs
                     {:head [:all]})]
  (let [clean-tree (add-to-bnf-file (process-bnf-file tree))]
    ((create-metaparser clean-tree) testfile))))
