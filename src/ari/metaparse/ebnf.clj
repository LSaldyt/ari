(ns ari.metaparse.ebnf 
  (:require [ari.lex :refer [lex]]
            [ari.parse.parse :refer :all]
            [ari.parse.base :refer :all]
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

(def terminal   (tag :string   :terminal))
(def special    (tag "special" :special))
(def identifier (tag "name"    :identifier))

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

(defparser alt-element (from-except elements alternation))

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

(defparser con-element (from-except elements concatenation alternation))

(defparser concatenation (sep-by1 con-element (white (token ","))))

; So that elements is a list of legit, defined functions
(def elements [concatenation
               grouping
               repetition
               optional-form
               alternation
               special
               terminal
               identifier])

(def special-separators [["\"" "\"" :string] ["'" "'" :string] ["(*" "*)" :comment]])

(declare process-ebnf-element)

(defn break-tree [single-tree]
  (let [k (first (keys single-tree))]
    [k (k single-tree)]))

(defn process-concatenation [element ptree]
  (let [values (map :con-element (:values element))]
    (let [elements (map #(process-ebnf-element % ptree) values)]
      (conseq-merge elements))))

(defn process-alternation [element ptree]
  (let [values (map :alt-element (:values element))]
    (from (map #(process-ebnf-element % ptree) values))))

(defn process-repetition [element ptree]
  (many (process-ebnf-element (:element element) ptree)))

(defn process-terminal [element ptree]
  (let [item (first element)]
      (token item item)))

(defn- replace-special [item]
  (cond (= item "NEWLINE")
        "\n"
        (= item "SPACE")
        " "
        (= item "TAB")
        "\t"
        :else
        item))

(defn process-special [element ptree]
  (token (replace-special (first element))))

(defn process-ref [element ptree]
  (let [k (first (:identifier element))]
    (retrieve k ptree)))

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
          (= k :special)
          (process-special tree ptree)
          (= k :identifier)
          (process-ref element ptree)
          :else
          element)))

(defn get-identifier [definition]
  (first (:identifier definition)))

(defn process-ebnf-tree [tree]
  (let [values (:values tree)]
    (def parser-tree (atom {}))
    (let [new-tree (into {} 
            (for [definition values]
              (let [definition (:definition definition)
                    identifier (get-identifier definition)]
                [identifier
                 (create-parser 
                   identifier
                   (process-ebnf-element (:element definition) parser-tree))])))]
      (reset! parser-tree new-tree))))

(def special-separators [["\"" "\"" :string] ["'" "'" :string] ["#" "\n" :comment]])

(def tag-pairs [[#"NEWLINE" "special"]
                [#"[_a-zA-Z][_a-zA-Z0-9]{0,30}" "name"]
                [#"'" "quote"]
                [#"\n" "newline"]
                [#" " "space"]
                [#"\\|" "operator"]
                [#":" "colon"]])

(defn create-ebnf-metaparser [tree]
  (fn [filename] (read-source filename
                              (get tree "body")
                              [];[" " "(" ")" "\n"]
                              [];special-separators
                              [];tag-pairs
                              {:head [:all]})))

(defn ebnf [filename]
  (let [[[tree remaining ebnf-log] log]
        (read-source filename 
                     (many definition)
                     separators 
                     special-separators
                     tag-pairs
                     {:head [:all]})]
    ; (println "Log:")
    ; (clojure.pprint/pprint log)
    ; (println "EBNF Log:")
    ; (clojure.pprint/pprint ebnf-log)
    (println "Tree:")
    (clojure.pprint/pprint tree)
    (println "Remaining:")
    (clojure.pprint/pprint remaining)
    (let [clean-tree (process-ebnf-tree tree)]
      (println "Result:")
      (clojure.pprint/pprint clean-tree)
      (create-ebnf-metaparser clean-tree))))
