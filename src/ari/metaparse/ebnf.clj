(ns ari.metaparse.ebnf 
  (:require [ari.lex :refer [lex]]
            [ari.parse :refer :all]
            [ari.translate :refer [read-source]]))

; ISO/IEC 14977 standard for EBNF
;
; x definition 	=
; x concatenation 	,
; x termination 	;
; x alternation 	|
; x optional 	[ ... ]
; x repetition 	{ ... }
; x grouping 	( ... )
; x terminal string 	" ... "
; x terminal string 	' ... '
;   comment 	(* ... *)
;   special sequence 	? ... ?
;   exception 	-

(def separators [":" " " "(" ")" "{" "}" "'" "|" "\n" "." "!" "`" "@" "," "=" "\"" ";"])
(def tag-pairs [[#"^([A-Za-z]|[0-9]|_)+$" "name"]
                [#"'" "quote"]
                [#"\"" "quote"]
                [#"=" "operator"]
                [#"\n" "newline"]
                [#" " "space"]
                [#"\\|" "operator"]
                [#":" "colon"]])

(def whitespace (optional (discard (many (from [(tag "space") (tag "newline")])))))

(defn white [parser]
  (discard (conseq-merge [whitespace parser whitespace])))

(defparser terminal (tag :string :string))
(defparser identifier (tag "name" :name))

(declare alternation)
(declare concatenation)
(declare grouping)
(declare repetition)
(declare optional-form)
(declare elements)

(defparser element (from elements))

(defparser definition (conseq-merge
                        [identifier
                         whitespace
                         (token "=")
                         whitespace
                         element
                         whitespace
                         (token ";")
                         whitespace]))

(defparser alt-element (any-except elements alternation))

(defparser alternation (sep-by1 alt-element (white (token "|"))))

(defparser optional-form (conseq-merge 
                           [(token "[")
                            whitespace
                            terminal
                            whitespace
                            (token "]")]))

(defparser repetition (conseq-merge
                        [(token "{")
                         whitespace
                         element
                         whitespace
                         (token "}")]))

(defparser grouping (conseq-merge
                      [(token "(")
                       whitespace
                       element
                       whitespace
                       (token ")") ]))

(defparser con-element (any-except elements concatenation alternation))

(defparser concatenation (sep-by1 con-element (white (token ","))))

; So that elements is a list of legit, defined functions
(def elements [grouping
               repetition
               optional-form
               alternation
               concatenation
               terminal
               identifier])

(def special-separators [["\"" "\"" :string] ["'" "'" :string] ["(*" "*)" :comment]])

;(declare alternation)
;(declare concatenation)
;(declare grouping)
;(declare repetition)
;(declare optional-form)
;(declare elements)

(declare process-ebnf-element)

(defn break-tree [single-tree]
  (let [k (first (keys single-tree))]
    [k (k single-tree)]))

(defn process-concatenation [element ptree]
  (let [values (map :con-element (:values element))]
    (let [elements (map #(process-ebnf-element % ptree) values)]
      (println "Elements")
      (clojure.pprint/pprint elements)
      (conseq-merge elements))))

(defn process-alternation [element ptree]
  (let [values (map :alt-element (:values element))]
    (from (map #(process-ebnf-element % ptree) values))))

(defn process-repetition [element ptree]
  (many (process-ebnf-element (:element element) ptree)))

(defn process-terminal [element ptree]
  (token (first (:string (:terminal element)))))

(defn process-ref [element ptree]
  (let [k (first (:name (:identifier element)))]
    (fn [tokens] ((get @ptree k) tokens))))

(defn process-ebnf-element [element ptree]
  (let [[k tree] (break-tree element)]
    (cond (= k :alternation)
          (process-alternation tree ptree)
          (= k :concatenation)
          (process-concatenation tree ptree)
          (= k :repetition)
          (process-repetition tree ptree)
          (= k :terminal)
          (process-terminal tree ptree)
          (= k :identifier)
          (process-ref element ptree)
          :else
          (do
            (println "Non-matching element:")
            (println element)
            element))))

(defn get-identifier [definition]
  (first (:name (:identifier definition))))

(defn process-ebnf-tree [tree]
  (let [values (:values tree)]
    (def parser-tree (atom {}))
    (let [new-tree (into {} 
            (for [definition values]
              (let [definition (:definition definition)]
                [(get-identifier definition) 
                 (process-ebnf-element (:element definition) parser-tree)])))]
      (reset! parser-tree new-tree))))

(def special-separators [["\"" "\"" :string] ["'" "'" :string] ["#" "\n" :comment]])

(defn create-ebnf-metaparser [tree]
  (fn [filename] (read-source filename
                              (get tree "body")
                              [" " "(" ")" "\n"]
                              special-separators
                              [])))

(defn ebnf [filename]
  (let [[tree remaining] 
        (read-source filename 
                     (many definition)
                     separators 
                     special-separators
                     tag-pairs)]
    (let [clean-tree (process-ebnf-tree tree)]
      (clojure.pprint/pprint clean-tree)
      (println (get clean-tree "body"))
      (create-ebnf-metaparser clean-tree))))
