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
                [#"\n" "newline"]
                [#" " "space"]
                [#"\\|" "operator"]
                [#":" "colon"]])
(def whitespace (optional (discard (many (any-of [(token " ") (token "\n")])))))

(defparser definition (psequence-merge
                        [(tag "name" :name)
                         (token "=")
                         parser-part
                         (token ";")
                         whitespace
                         ]))

(defparser alternation (sep-by1 parser-part (token "|")))

(defparser optional-form (psequence-merge 
                           [(token "[")
                            whitespace
                            parser-part
                            whitespace
                            (token "]")]))

(defparser repetition (psequence-merge
                        [(token "{")
                         whitespace
                         parser-part
                         whitespace
                         (token "}")]))

(defparser grouping (psequence-merge
                      [(token "(")
                       whitespace
                       parser-part
                       whitespace
                       (token ")") ]))

(declare terminal)

(defparser concatenation (sep-by1 terminal (token ",")))

;(defparser concatenation (psequence-merge [terminal (token ",") terminal]))

(defn make-terminal [q]
  (psequence-merge
    [(token q)
     (token any :string)
     (token q)]))

(defparser terminal (any-of [(make-terminal "\"") (make-terminal "'")]))

(defparser parser-part (any-of 
                         [;alternation
                          ;optional-form
                          ;repetition
                          ;grouping
                          concatenation
                          terminal
                          ]))
(defn ebnf [filename]
  (let [[tree remaining] 
        (read-source filename 
                     (many definition)
                     separators 
                     tag-pairs)]
    [tree remaining]))
