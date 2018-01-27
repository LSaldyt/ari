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
(declare terminal)

(def separators [":" " " "(" ")" "{" "}" "'" "|" "\n" "." "!" "`" "@" "," "=" "\""])
(def tag-pairs [[#"[_a-zA-Z][_a-zA-Z0-9]{0,30}" "name"]
                [#"'" "quote"]
                [#"\"" "quote"]
                [#"\n" "newline"]
                [#" " "space"]
                [#"\\|" "operator"]
                [#":" "colon"]])

(def whitespace (optional (discard (many (any-of [(tag "space") (tag "newline")])))))

(defparser definition (psequence-merge
                        [(tag "name" :name)
                         whitespace
                         (token "=")
                         whitespace
                         parser-part
                         whitespace
                         (token ";")
                         whitespace
                         ]))

(defparser alternation (sep-by1 terminal 
                                (discard 
                                  (psequence-merge [whitespace (token "|") whitespace]))))

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
                         terminal
                         whitespace
                         (token "}")]))

(defparser grouping (psequence-merge
                      [(token "(")
                       whitespace
                       terminal
                       whitespace
                       (token ")") ]))

(defparser concatenation (sep-by1 terminal (token ",")))

(defparser terminal (tag :string :string))

(defparser parser-part (any-of 
                         [alternation
                          optional-form
                          repetition
                          grouping
                          concatenation
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
