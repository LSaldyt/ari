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

(defn ebnf [filename]
  (let [[tree remaining] 
        (read-source filename 
                     (many definition)
                     separators 
                     special-separators
                     tag-pairs)]
    [tree remaining]))
