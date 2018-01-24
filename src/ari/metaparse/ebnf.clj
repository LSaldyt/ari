(ns ari.metaparse.ebnf 
  (:require [ari.lex :refer [lex]]
            [ari.parse :refer :all]
            [ari.translate :refer [read-source]]))

; ISO/IEC 14977 standard for EBNF
;
; definition 	=
; concatenation 	,
; termination 	;
; alternation 	|
; optional 	[ ... ]
; repetition 	{ ... }
; grouping 	( ... )
; terminal string 	" ... "
; terminal string 	' ... '
; comment 	(* ... *)
; special sequence 	? ... ?
; exception 	-

; (defparser direct-token (psequence-merge
;                           [(token "'") 
;                            (tag "name" :token) 
;                            (optional (tag "space"))
;                            (optional (tag "name" :tag)) 
;                            (token "'")]))


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
                         whitespace
                         (token "=")
                         whitespace
                         parser-part
                         whitespace
                         (token ";")
                         whitespace
                         ]))

(defparser parser-part (psequence-merge
                         [(token "\"")
                          (token any :string)
                          (token "\"")]))

(defn ebnf [filename]
  (let [[tree remaining] 
        (read-source filename 
                     definition
                     separators 
                     tag-pairs)]
    tree))
