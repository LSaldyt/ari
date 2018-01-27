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

(declare parser-part)

(def separators [":" " " "(" ")" "{" "}" "'" "|" "\n" "." "!" "`" "@" "," "=" "\""])
(def tag-pairs [[#"[_a-zA-Z][_a-zA-Z0-9]{0,30}" "name"]
                [#"'" "quote"]
                [#"\"" "quote"]
                [#"=" "operator"]
                [#"\n" "newline"]
                [#" " "space"]
                [#"\\|" "operator"]
                [#":" "colon"]])

(def whitespace (optional (discard (many (any-of [(tag "space") (tag "newline")])))))

(defn white [parser]
  (discard (psequence-merge [whitespace parser whitespace])))

(defparser terminal (tag :string :string))
(defparser identifier (tag "name" :name))

(declare alternation)
(declare concatenation)
(declare grouping)
(declare repetition)
(declare optional-form)

(defparser simple-element (any-of [terminal
                                   identifier]))

(defparser element (any-of [grouping
                            repetition
                            optional-form
                            alternation
                            concatenation
                            terminal 
                            identifier
                            ]))

(defparser definition (psequence-merge
                        [identifier
                         whitespace
                         (token "=")
                         whitespace
                         parser-part
                         whitespace
                         (token ";")
                         whitespace]))

(defparser alt-element (any-of [grouping
                            repetition
                            optional-form
                            concatenation
                            terminal 
                            identifier
                            ]))

(defparser alternation (sep-by1 alt-element (white (token "|"))))

(defparser optional-form (psequence-merge 
                           [(token "[")
                            whitespace
                            ;parser-part
                            terminal
                            whitespace
                            (token "]")]))

(defparser repetition (psequence-merge
                        [(token "{")
                         whitespace
                         ;parser-part
                         element
                         whitespace
                         (token "}")]))

(defparser grouping (psequence-merge
                      [(token "(")
                       whitespace
                       terminal
                       whitespace
                       (token ")") ]))

(defparser con-element (any-of [grouping
                            repetition
                            optional-form
                            terminal 
                            identifier
                            ]))

(defparser concatenation (sep-by1 con-element (white (token ","))))

(defparser parser-part (any-of 
                         [alternation
                          optional-form
                          repetition
                          grouping
                          concatenation
                          identifier
                          terminal
                          whitespace
                          ]))
(defn ebnf [filename]
  (let [[tree remaining] 
        (read-source filename 
                     (many definition)
                     separators 
                     tag-pairs)]
    [tree remaining]))
